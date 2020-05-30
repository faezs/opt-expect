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
import Data.Maybe (fromJust)

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

import System.Random.Shuffle
import Control.Monad.Random.Class (MonadRandom)

type MonadEnv m = (MonadSample m, MonadAsync m)

type EnvState m s r = (MonadSample m) => StateT s m r

data Transition s a r = Transition
  { s_t :: !s
  , s_tn :: !s
  , a_t :: !a
  , rw :: !r
  , done :: !Bool
  , v_t :: r
  , gAdv :: r
  } deriving (Eq, Ord, Show, Generic)


data Episode s a r = Episode
  { timesteps :: Int
  , reward :: r
  , trajectories :: [Transition s a r]
  , rewardToGo :: [r]
  , stateValue :: [r]
  , gAE :: [r]
  } deriving (Eq, Ord, Show, Generic)

instance Num r => Semigroup (Episode s a r) where
  a <> b = Episode
           { timesteps = timesteps a + timesteps b
           , reward = reward a + reward b
           , trajectories = trajectories a <> trajectories b
           , rewardToGo = rewardToGo a <> rewardToGo b
           , stateValue = stateValue a <> stateValue b
           , gAE = gAE a <> gAE b
           }


shuffleEpisodeM :: (MonadRandom m) => Episode s a r -> m (Episode s a r)
shuffleEpisodeM e@Episode{..} = do
    (t', r', s', g') <- unzip4 <$> shuffleM (zip4 trajectories rewardToGo stateValue gAE)
    return $ e{trajectories = t', rewardToGo = r', stateValue = s', gAE = g'}
    where
      zip4 (t:tx) (r:rx) (s:sx) (g:gx) = (t, r, s, g):(zip4 tx rx sx gx)
      zip4 [] _ _ _ = []
      zip4 _ [] _ _ = []
      zip4 _ _ [] _ = []
      zip4 _ _ _ [] = []
      unzip4 ((a, b, c, d):xs) = let
        (a', b', c', d') = unzip4 xs
        in (a:a', b:b', c:c', d:d')
      unzip4 [] = ([],[],[],[])
      

instance Num r => Monoid (Episode s a r) where
  mempty = Episode 0 0 [] [] [] []

runEpisode ::
  forall m s a r.
  ( Additive r,
    Fractional r,
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
  let
    (r2g, rwT) = rw2Go txs
    r2gTrajectories :: [Transition s a r]
    r2gTrajectories = ((\(t, r)-> t{rw=r}) <$> (zip txs r2g))
    gAEs = (advantage valueFn 0.2 0.9 r2gTrajectories)
    sV = (valueFn . s_t) <$> txs
    updatedTx = (\(t, g, sv)-> t{gAdv=g, v_t=sv}) <$> zip3 r2gTrajectories gAEs sV
  return $ Episode
    { timesteps = length txs
    , reward = rwT
    , trajectories = updatedTx
    , rewardToGo = r2g
    , stateValue = sV
    , gAE = gAEs
    }
  where
    rw2Go xs = let
      r2g = (\r -> totalR - r)
            <$> (scanl (\rAtT Transition{rw} -> rAtT + rw) 0 xs)
      totalR = sumA (fmap rw xs)
      in (r2g, totalR)
    takeWhileInclusive _ [] = []
    takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs else []
{-# INLINE runEpisode #-}


{--
--}

advantage :: (Num r) => (s -> r) -> r -> r -> [Transition s a r] -> [r]
advantage vf lambda gamma txs = scanl (+) 0 $ zipWith (oneStepTDResidual vf) [lambda * gamma ^ i | i <- [0,1..]] txs

oneStepTDResidual :: (Num r) => (s -> r) -> r -> Transition s a r -> r
oneStepTDResidual vf gamma Transition{..} = rw + (gamma * vf s_tn) - (vf s_t)

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
  ps <- S.fold FL.last --((\a b -> b)
                      -- <$> (deltaFold)
                      -- <*> (FL.Fold (\a b -> return $ b) (pure (polParams, vfParams)) pure)
                     --)
              $ S.take nEpochs
              $ S.iterateM (epoch nEps) (pure (polParams, vfParams))
  let (p', v') = fromJust ps
  return (p', v')
    where
      deltaFold :: FL.Fold IO (PType i h o, PType i h 1) ()
      deltaFold = FL.Fold wrapDelta begin unwrap
        where
          unwrap :: (PType i h o, PType i h 1) -> IO ()
          unwrap = (\_-> pure ())
          begin :: IO (PType i h o, PType i h 1)
          begin = pure (polParams, vfParams)
          wrapDelta :: (PType i h o, PType i h 1) -> (PType i h o, PType i h 1) -> IO (PType i h o, PType i h 1)
          wrapDelta (p, v) (p', v') = putStrLn ("average policy parameter delta: \n" <> (show $ sumA (p ^-^ p')))
                                      >> return (p', v')
      epoch :: Int -> (PType i h o, PType i h 1) -> IO (PType i h o, PType i h 1)
      epoch nEps (polP, vfP) = S.fold (comb
                                       <$> (policyFold @IO @s @a @i @h @o 80 learner polP)
                                       <*> (valueFnFold @IO @s @a @i @h 80 vfLearner vfP)
                                       <*> statisticsFold)
                         (episodes polP vfP nEps)
      comb :: PType i h o -> PType i h 1 -> () -> (PType i h o, PType i h 1)
      comb p v () = (p, v)
      episodes :: (IsStream t) => PType i h o -> PType i h 1 -> Int -> t IO (Episode s a R)
      episodes polP vfP nEps = serially
        -- $ S.mapM shuffleEpisodeM
        $ S.replicateM nEps
        ((\s -> sampleIO $
           runEpisode episodeFn (agent polP) ((wrapVF valueFn) vfP) s) =<< (sampleIO $ initStateM))
--{-# INLINE runEpochs #-}


policyFold :: forall m s a i h o. (MonadRandom m, MonadAsync m, HasV s i, HasV a o, KnownNat h)
  => Int
  -> (RLUpdate i h o s a)
  -> PType i h o
  -> FL.Fold m (Episode s a R) (PType i h o)
policyFold nIter = \policyUpdate initP -> FL.Fold (step policyUpdate) (pure initP) (finish)
  where
    step :: (Episode s a R -> Unop (PType i h o)) -> (PType i h o) -> Episode s a R -> m (PType i h o)
    step pu p ep = (return . fromJust) =<< (S.fold FL.last
                   $ S.take nIter
                   $ S.iterateM (\p' -> (\ep' -> return $ pu ep' p') =<< (shuffleEpisodeM ep)) (pure $ p))
    finish :: (PType i h o) -> m (PType i h o)
    finish = return
--{-# INLINE policyFold #-}


valueFnFold :: forall m s a i h . (MonadRandom m, MonadAsync m, HasV s i, KnownNat h)
  => Int
  -> (RLUpdate i h 1 s a)
  -> PType i h 1
  -> FL.Fold m (Episode s a R) (PType i h 1)
valueFnFold nIter = \valueUpdate initP -> FL.Fold (step valueUpdate) (pure initP) finish
  where
    step :: (Episode s a R -> Unop (PType i h 1)) -> (PType i h 1) -> Episode s a R -> m (PType i h 1)
    step vu p ep = (return . fromJust) =<< (S.fold FL.last
                   $ S.take nIter
                   $ S.iterateM (\p' -> (\ep' -> return $ vu ep' p') =<< (shuffleEpisodeM ep)) (pure $ p)) 
    finish :: PType i h 1 -> m (PType i h 1)
    finish = return
--{-# INLINE valueFnFold #-}


statisticsFold :: forall m s a. (MonadIO m) => FL.Fold m (Episode s a R) ()
statisticsFold = FL.Fold (step) begin end
  where
    begin = pure (0, 0, 0)
    step :: (R, R, R) -> Episode s a R -> m (R, R, R)
    step (rTotal, svTotal, len) Episode{..} = return $ ((reward + rTotal), (svTotal + sumA (stateValue ^-^ rewardToGo)), len + 1)
    end (rTotal, svTotal, len) = (liftIO . putStrLn $
                        "average reward: " <> (show $ round $ rTotal / len)
                        <> " average sv-error: " <> (show $ round $ svTotal / len) 
                        <> " nEpisodes : " <> (show . round $ len))
--{-# INLINE statisticsFold #-}


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
