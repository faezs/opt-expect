{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
module Env where

import Prelude hiding (length, zip, zipWith)

import ConCat.Misc
import ConCat.Additive

import Control.Monad.State.Lazy
import Control.Monad
import Control.Monad.IO.Class

import Data.Key
import Data.Foldable
import Data.Traversable

import GHC.Generics hiding (R)
import GHC.TypeLits

import Utils
import MLUtils (HasLayers(..), showPart)
import ConCat.Deep


import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler

import qualified Streamly.Prelude as S
import Streamly
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL
import qualified Data.Vector.Sized as VS


type MonadEnv m = (MonadSample m, MonadAsync m)

type EnvState m s r = (MonadSample m) => StateT s m r

data Transition s a r = Transition
  { s_t :: s
  , s_tn :: s
  , a_t :: a
  , rw :: r
  , done :: Bool
  } deriving (Eq, Ord, Show, Generic)


data Episode s a r = Episode
  { timesteps :: Int
  , reward :: r
  , trajectories :: [Transition s a r]
  , rewardToGo :: [r]
  , stateValue :: [r]
  } deriving (Eq, Ord, Show, Generic)

instance Num r => Semigroup (Episode s a r) where
  a <> b = Episode
           { timesteps = timesteps a + timesteps b
           , reward = reward a + reward b
           , trajectories = trajectories a <> trajectories b
           , rewardToGo = rewardToGo a <> rewardToGo b
           , stateValue = stateValue a <> stateValue b
           }

instance Num r => Monoid (Episode s a r) where
  mempty = Episode 0 0 [] [] []

runEpisode ::
  forall m s a r.
  ( Additive r,
    MonadSample m,
    Num r
  )
  => ((s -> m a) -> EnvState m s (Transition s a r))
  -> (s -> m a)
  -> (s -> r)
  -> s
  -> m (Episode s a r)
runEpisode stepFn agent valueFn initS = do
  txs <- (takeWhileInclusive (\tx -> not . done $ tx))
           <$> (evalStateT (mapM (\_ -> stepFn agent) [1..1000]) $ initS)
  return $ Episode
    { timesteps = length txs
    , reward = sumA (fmap rw txs)
    , trajectories = txs
    , rewardToGo = rTG txs
    , stateValue = (valueFn . s_t) <$> txs
    }
  where
    rTG xs = (\r -> totalR - r)
                   <$> (scanl (\rAtT Transition{rw} -> rAtT + rw) 0 xs)
      where
        totalR = sumA (fmap rw xs)
    takeWhileInclusive _ [] = []
    takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs else []
{-# INLINE runEpisode #-}



type RLUpdate i h o s a = Episode s a R -> Unop (PType i h o)

type RunEnv s a = ((s -> SamplerIO a)) -> EnvState SamplerIO s (Transition s a R)

type StochasticPolicy i h o s a = (PType i h o -> s -> SamplerIO a)

type ValueFn i h = (PType i h 1 -> V i R -> V 1 R)

runEpochs :: forall s a i h o. (HasV s i, HasV a o, KnownNat h, Show s, Show a)
  => Int
  -> Int
  -> (RLUpdate i h o s a)
  -> (RLUpdate i h 1 s a)
  -> (RunEnv s a)
  -> (SamplerIO s)
  -> StochasticPolicy i h o s a
  -> ValueFn i h
  -> PType i h o
  -> PType i h 1
  -> IO (PType i h o, PType i h 1)
runEpochs nEpochs nEps learner vfLearner episodeFn initStateM agent valueFn polParams vfParams = do
  (p', v') <- S.fold ((\a b -> b)
                      <$> (deltaFold)
                      <*> (FL.Fold (\a b -> return $ b) (pure (polParams, vfParams)) pure)
                     )
              $ S.take nEpochs
              $ S.iterateM (epoch nEps) (pure (polParams, vfParams))
  return (p', v')
    where
      deltaFold :: FL.Fold IO (PType i h o, PType i h 1) ()
      deltaFold = FL.Fold wrapDelta begin unwrap-- unwrap
        where
          unwrap :: (PType i h o, PType i h 1) -> IO ()
          unwrap = (\_-> pure ())
          begin :: IO (PType i h o, PType i h 1)
          begin = pure (polParams, vfParams)
          wrapDelta :: (PType i h o, PType i h 1) -> (PType i h o, PType i h 1) -> IO (PType i h o, PType i h 1)
          wrapDelta (p, v) (p', v') = print (sumA $ p ^-^ p') >> return (p', v')
      epoch :: Int -> (PType i h o, PType i h 1) -> IO (PType i h o, PType i h 1)
      epoch nEps (polP, vfP) = S.fold (comb
                                       <$> (policyFold @IO @s @a @i @h @o learner polP)
                                       <*> (valueFnFold @IO @s @a @i @h vfLearner vfP)
                                       <*> statisticsFold)
                         (episodes polP vfP nEps)
      comb :: PType i h o -> PType i h 1 -> () -> (PType i h o, PType i h 1)
      comb p v () = (p, v)
      episodes :: (IsStream t) => PType i h o -> PType i h 1 -> Int -> t IO (Episode s a R)
      episodes polP vfP nEps = parallely $ S.replicateM nEps
        ((\s -> sampleIO $
           runEpisode episodeFn (agent polP) ((wrapVF valueFn) vfP) s) =<< (sampleIO $ initStateM))
{-# INLINE runEpochs #-}


policyFold :: forall m s a i h o. (Monad m, HasV s i, HasV a o, KnownNat h)
  => (RLUpdate i h o s a)
  -> PType i h o
  -> FL.Fold m (Episode s a R) (PType i h o)
policyFold = \policyUpdate initP -> FL.Fold (step policyUpdate) (pure initP) (finish)
  where
    step :: (Episode s a R -> Unop (PType i h o)) -> (PType i h o) -> Episode s a R -> m (PType i h o)
    step pu p ep = pure $ pu ep p
    finish :: (PType i h o) -> m (PType i h o)
    finish = return
{-# INLINE policyFold #-}


valueFnFold :: forall m s a i h . (Monad m, HasV s i, KnownNat h)
  => (RLUpdate i h 1 s a)
  -> PType i h 1
  -> FL.Fold m (Episode s a R) (PType i h 1)
valueFnFold = \valueUpdate initP -> FL.Fold (step valueUpdate) (pure initP) finish
  where
    step :: (Episode s a R -> Unop (PType i h 1)) -> (PType i h 1) -> Episode s a R -> m (PType i h 1)
    step vu p ep = pure $ vu ep p
    finish :: PType i h 1 -> m (PType i h 1)
    finish = return
{-# INLINE valueFnFold #-}

statisticsFold :: forall m s a. (MonadIO m) => FL.Fold m (Episode s a R) ()
statisticsFold = FL.Fold (step) begin end
  where
    begin = pure (0, 0)
    step :: (R, R) -> Episode s a R -> m (R, R)
    step (total, len) Episode{..} = return $ ((reward + total), len + 1)
    end (total, len) = (liftIO . putStrLn $ "average reward: " <> (show $ total / len) <> " nEpisodes : " <> (show . round $ len))
{-# INLINE statisticsFold #-}


wrapVF :: (HasV s i, KnownNat h) => (PType i h 1 -> V i R -> V 1 R) -> (PType i h 1 -> s -> R)
wrapVF vf = \p s -> VS.sum $ vf p (toV s)
{-# INLINE wrapVF #-}


weightDelta :: [PType i h o] -> [PType i h 1] -> IO ()
weightDelta ps vs = do
  mapM_ putStrLn $ showPart getWeights "Policy" ps
  mapM_ putStrLn $ showPart getWeights "Value Function" $ vs
  return ()
{-# INLINE weightDelta #-}

{--
      printMS :: forall f a. (Show a, Functor f, Show (f a)) => String -> (f a, f a) -> String
      printMS = (\tag (m, s) -> tag <> "\n" <> "mean delP: " <> show m <> "\n" <> "std delP: " <> show s)
      meanStd :: f (g a) -> f (g a) -> (f a, f a)
      meanStd a a' = (mean dp, stdDev dp)
        where
          dp = a ^-^ a'
          mean :: f (g a) -> f a
          mean fs = zipWith (/) (fmap sumA fs) (fmap (fromIntegral length) fs)
          stdDev :: f (g a) -> f a
          stdDev fs = (fmap (\f-> f ^-^ mean f) fs) ^/ (sumA $ mean fs)
--}
