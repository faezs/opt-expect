{-# LANGUAGE MultiParamTypeClasses #-}
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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Env where

import Prelude hiding (length, zip, zipWith)

import ConCat.Misc
import ConCat.Additive

import Control.Monad.State.Strict
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
import qualified Streamly.Internal.Prelude as S
import Streamly
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL
import qualified Data.Vector.Sized as VS

import System.Random.Shuffle
import Control.Monad.Random.Class (MonadRandom)
import Control.Monad.Base
import Control.Monad.Trans.Control

newtype MonadEnv a = MonadEnv { runMonadEnv :: SamplerIO a } deriving (Functor, Applicative, Monad)

--instance MonadBase MonadEnv MonadEnv where
--  liftBase = id-- sampleIO . runMonadEnv

instance MonadBase IO MonadEnv where
  liftBase = undefined

instance MonadBaseControl IO MonadEnv where
  liftBaseWith = undefined
  restoreM = undefined

type EnvState m s r = (MonadSample m) => StateT s m r

type RLUpdate n i h o s a = V n (Transition s a R) -> Unop (PType i h o)

type RunEnv s a = ((s -> SamplerIO a)) -> EnvState SamplerIO s (Transition s a R)

type StochasticPolicy i h o s a = (PType i h o -> s -> SamplerIO a)

type ValueFn i h = (PType i h 1 -> V i R -> V 1 R)




data Transition s a r = Transition
  { s_t :: !s
  , s_tn :: !s
  , a_t :: !a
  , rw :: !r
  , done :: !Bool
  , stateValue :: !r
  , advantage :: !r
  , tdRes :: !r
  } deriving (Eq, Ord, Show, Generic)

emptyTransition :: (Monoid s, Monoid a, Num r) => Transition s a r
emptyTransition = Transition mempty mempty mempty 0 False 0 0 0

{--
data Episode s a r = Episode
  { timesteps :: !Int
  , reward :: !r
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
--}


type EnvCon m s a r = (MonadSample m, MonadAsync m, Monoid s, Monoid a, Num r, Fractional r) 

runEpisode ::
  forall m s a r.
  ( EnvCon m s a r
  , MonadSample m
  , MonadAsync m
  )
  => ((s -> m a) -> EnvState m s (Transition s a r))
  -> (s -> m a)
  -> (s -> r)
  -> s
  -> SerialT m (Transition s a r)
-- Changes:
-- 1) Accumulating scan over Transition Rewards then subtraction from total to get rewardToGo and put that inside the transitions
-- 2) Generalized Advantage and state value scan over transitions
runEpisode stepFn agent valueFn initS = let
  episode :: SerialT m (Transition s a r)
  episode = (S.takeWhile (\tx -> not . done $ tx))
            $ (S.evalStateT initS (S.mapM (\_ -> stepFn agent) $ S.enumerateFromTo (1 :: Int) 1000))
  epWithR2G = rw2Go episode
  epWithAdvantage = (advantageF valueFn 0.99 0.2) epWithR2G 
  in epWithAdvantage   
{-# INLINE runEpisode #-}


rw2Go :: forall m s a r. (EnvCon m s a r) => SerialT m (Transition s a r) -> SerialT m (Transition s a r)
rw2Go xs = S.postscan (subRwByTotal <$> sumRw <*> accumRw) xs
  where
    sumRw :: FL.Fold m (Transition s a r) r
    sumRw = FL.Fold (\x y -> return $ (x + rw y)) (pure $ 0) (pure)
    subRwByTotal :: r -> Transition s a r -> Transition s a r
    subRwByTotal tot t1 = t1{rw = tot - rw t1}
    accumRw :: FL.Fold m (Transition s a r) (Transition s a r)
    accumRw = FL.Fold step (pure emptyTransition) (pure)
      where
        step t0 t1 = return $ t1{rw = rw t0 + rw t1}
{-# INLINE rw2Go #-}

advantageF :: forall t m s a r. (IsStream t, EnvCon m s a r) => (s -> r) -> r -> r -> t m (Transition s a r) -> t m (Transition s a r)
advantageF vf lambda gamma txs = S.scanl' (accAdv) emptyTransition $ S.zipWith (oneStepTDResidual vf) ((\i -> lambda * gamma ^ i) <$>  S.enumerateFrom (0 :: Int)) txs
  where
    accAdv t0@Transition{advantage} t1@Transition{tdRes} = t1{advantage = advantage + tdRes}
{-# INLINE advantageF #-}

oneStepTDResidual :: (Num r) => (s -> r) -> r -> Transition s a r -> Transition s a r
oneStepTDResidual vf gamma t@Transition{..} = t{tdRes = rw + (gamma * vf s_tn) - (v_t), stateValue= v_t}
  where
    v_t = vf s_tn
{-# INLINE oneStepTDResidual #-}

{--
newtype Policy t m s a r i h o = Policy (t m (Transition s a R) -> PType i h o -> m (PType i h o))

runPolicy :: Policy t m s a r i h o -> t m (Transition s a R) -> PType i h o -> m PType i h o
runPolicy (Policy policyFn) episode initP = policyFn episode initP 

epoch :: Int -> (PType i h o, PType i h 1) -> IO (PType i h o, PType i h 1)
epoch nEps (polP, vfP) = S.fold (comb
                                 <$> (policyFold @IO @s @a @i @h @o 80 learner polP)
                                 <*> (valueFnFold @IO @s @a @i @h 80 vfLearner vfP)
                                 <*> statisticsFold)


learn  :: forall m n i h o s a.
  (RLCon s a i h o, KnownNat n, MonadAsync m)
  => Int
  -> R
  -> R
  -> SerialT m (Transition s a R)
  -> PType i h o
  -> m (PType i h o)
learn = undefined

runEpochs :: forall t m n s a i h o.
  ( HasV s i, HasV a o, KnownNat h
  , KnownNat n, Show s, Show a
  , IsStream t, MonadSample m, MonadAsync m
  )
  => Int
  -> Int
  -> (RLUpdate n i h o s a)
  -> (RLUpdate n i h 1 s a)
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
      
                         (episodes polP vfP nEps)
      comb :: PType i h o -> PType i h 1 -> () -> (PType i h o, PType i h 1)
      comb p v () = (p, v)
      episodes :: (IsStream t) => PType i h o -> PType i h 1 -> Int -> t IO (Transition s a R)
      episodes polP vfP nEps = serially
        -- $ S.mapM shuffleEpisodeM
        $ S.replicateM nEps
        ((\s -> sampleIO $
           runEpisode episodeFn (agent polP) ((wrapVF valueFn) vfP) s) =<< (sampleIO $ initStateM))
--{-# INLINE runEpochs #-}


policyFold :: forall m n s a i h o. (MonadRandom m, MonadAsync m, HasV s i, HasV a o, KnownNat h, KnownNat n)
  => Int
  -> (RLUpdate n i h o s a)
  -> PType i h o
  -> FL.Fold m (V n (Transition s a R)) (PType i h o)
policyFold nIter = \policyUpdate initP -> FL.Fold (step policyUpdate) (pure initP) (finish)
  where
    step :: (t m (Transition s a R) -> Unop (PType i h o)) -> (PType i h o) -> t m (Transition s a R) -> m (PType i h o)
    step pu p ep = (return . fromJust) =<< (S.fold FL.last
                   $ S.take nIter
                   $ S.iterateM (\p' -> (\ep' -> return $ pu ep' p') =<< (shuffleEpisodeM ep)) (pure $ p))
    finish :: (PType i h o) -> m (PType i h o)
    finish = return
--{-# INLINE policyFold #-}


valueFnFold :: forall m n s a i h . (MonadRandom m, MonadAsync m, HasV s i, KnownNat h, KnownNat n)
  => Int
  -> (RLUpdate n i h 1 s a)
  -> PType i h 1
  -> FL.Fold m (V n (Transition s a R)) (PType i h 1)
valueFnFold nIter = \valueUpdate initP -> FL.Fold (step valueUpdate) (pure initP) finish
  where
    step :: (t m (Transition s a R) -> Unop (PType i h 1)) -> (PType i h 1) -> t m (Transition s a R) -> m (PType i h 1)
    step vu p ep = (return . fromJust) =<< (S.fold FL.last
                   $ S.take nIter
                   $ S.iterateM (\p' -> (\ep' -> return $ vu ep' p') =<< (shuffleEpisodeM ep)) (pure $ p)) 
    finish :: PType i h 1 -> m (PType i h 1)
    finish = return
--{-# INLINE valueFnFold #-}


statisticsFold :: forall m s a. (MonadIO m) => FL.Fold m (Transition s a R) ()
statisticsFold = FL.Fold (step) begin end
  where
    begin = pure (0, 0, 0)
    step :: (R, R, R) -> t m (Transition s a R) -> m (R, R, R)
    step (rTotal, svTotal, len) tx = return $ ((reward + rTotal), (svTotal + sumA (stateValue ^-^ rewardToGo)), len + 1)
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
--}
