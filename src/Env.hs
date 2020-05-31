{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import Data.Default

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


import Immutable.Shuffle
import Control.Monad.Random.Class (MonadRandom)
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Catch


newtype MonadEnv a = MonadEnv { runMonadEnv :: SamplerIO a } deriving (Functor, Applicative, Monad, MonadIO, Generic)

instance MonadThrow MonadEnv where
  throwM = MonadEnv . liftIO . throwM


instance MonadBase IO MonadEnv where
  liftBase = MonadEnv . liftIO

instance MonadBaseControl IO MonadEnv where
  type StM MonadEnv a = a
  -- liftBaseWith :: ((forall a. m a -> IO (StM m a)) -> IO a) -> m a
  liftBaseWith f = liftIO $ liftBaseWith @IO @IO (\run -> f (run . sampleIO . runMonadEnv)) 
  restoreM a = liftIO $ restoreM @IO @IO a

deriving instance MonadSample MonadEnv


type EnvState m s r = (MonadSample m) => StateT s m r

type RLUpdate n i h o s a = V n (Transition s a R) -> Unop (PType i h o)

type RunEnv s a = ((s -> MonadEnv a)) -> EnvState MonadEnv s (Transition s a R)

type StochasticPolicy i h o s a = (PType i h o -> s -> MonadEnv a)

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

instance (Default s, Default a, Num r) => Default (Transition s a r) where
  def = Transition def def def 0 False 0 0 0


type EnvCon m s a r = (MonadSample m, MonadAsync m, Default s, Default a, Num r, Fractional r) 

rw2Go :: forall m s a r. (EnvCon m s a r) => SerialT m (Transition s a r) -> SerialT m (Transition s a r)
rw2Go xs = S.postscan (subRwByTotal <$> sumRw <*> accumRw) xs
  where
    sumRw :: FL.Fold m (Transition s a r) r
    sumRw = FL.Fold (\x y -> return $ (x + rw y)) (pure $ 0) (pure)
    subRwByTotal :: r -> Transition s a r -> Transition s a r
    subRwByTotal tot t1 = t1{rw = tot - rw t1}
    accumRw :: FL.Fold m (Transition s a r) (Transition s a r)
    accumRw = FL.Fold step (pure def) (pure)
      where
        step t0 t1 = return $ t1{rw = rw t0 + rw t1}
{-# INLINE rw2Go #-}

advantageF :: forall t m s a r. (IsStream t, EnvCon m s a r) => (s -> r) -> r -> r -> t m (Transition s a r) -> t m (Transition s a r)
advantageF vf lambda gamma txs = S.scanl' (accAdv) def $ S.zipWith (oneStepTDResidual vf) ((\i -> lambda * gamma ^ i) <$>  S.enumerateFrom (0 :: Int)) txs
  where
    accAdv t0@Transition{advantage} t1@Transition{tdRes} = t1{advantage = advantage + tdRes}
{-# INLINE advantageF #-}

oneStepTDResidual :: (Num r) => (s -> r) -> r -> Transition s a r -> Transition s a r
oneStepTDResidual vf gamma t@Transition{..} = t{tdRes = rw + (gamma * vf s_tn) - (v_t), stateValue= v_t}
  where
    v_t = vf s_tn
{-# INLINE oneStepTDResidual #-}



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
runEpisode stepFn agent valueFn initS = let
  episode :: SerialT m (Transition s a r)
  episode = (S.takeWhile (\tx -> not . done $ tx))
            $ (S.evalStateT initS (S.mapM (\_ -> stepFn agent) $ S.enumerateFromTo (1 :: Int) 1000))
  epWithR2G = rw2Go episode
  epWithAdvantage = (advantageF valueFn 0.99 0.2) epWithR2G 
  in epWithAdvantage   
{-# INLINE runEpisode #-}



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
  -> (MonadEnv s)
  -> StochasticPolicy i h o s a
  -> ValueFn i h
  -> PType i h o
  -> PType i h 1
  -> IO (PType i h o, PType i h 1)
runEpochs nEpochs nEps learner vfLearner episodeFn initStateM agent valueFn polParams vfParams = do
  ps <- S.fold (minibatchLearn nEps) (pure (polParams, vfParams))
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
      episodes :: (IsStream t) => PType i h o -> PType i h 1 -> Int -> t MonadEnv (Transition s a R)
      episodes polP vfP nEps = (\s -> runEpisode episodeFn (agent polP) ((wrapVF valueFn) vfP) s)
                                =<< (S.replicateM nEps initStateM)
--{-# INLINE runEpochs #-}



minibatchLearn :: forall m n s a i h o. (MonadAsync m, HasV s i, HasV a o, KnownNat h, KnownNat n)
  => Int
  -> (RLUpdate n i h o s a)
  -> PType i h o
  -> FL.Fold m (V n (Transition s a R)) (PType i h o)
minibatchLearn nIter = \updateFn initP -> FL.Fold (step updateFn) (pure initP) (finish)
  where
    step :: (V n (Transition s a R) -> Unop (PType i h o)) -> (PType i h o) -> V n (Transition s a R) -> m (PType i h o)
    step pu p ep = (return . fromJust) =<< (S.fold FL.last
                   $ S.take nIter
                   $ S.iterateM (\p' -> (\ep' -> return $ pu ep' p')
                                  =<< (shuffleSized ep))
                                             (pure $ p))
    finish :: (PType i h o) -> m (PType i h o)
    finish = return
{-# INLINE minibatchLearn #-}


shuffleSized :: forall m n a. (MonadIO m, KnownNat n) => V n a -> m (V n a)
shuffleSized v = (return . fromJust . VS.toSized @n) =<< (liftIO . shuffleM . VS.fromSized $ v)
{-# INLINE shuffleSized #-}


minibatchStatistics :: forall m s a. (MonadIO m) => FL.Fold m (Transition s a R) ()
minibatchStatistics = FL.Fold (step) begin end
  where
    begin = pure (0, 0, 0)
    step :: (R, R, R) -> (V n (Transition s a R)) -> m (R, R, R)
    step (rTotal, svTotal, len) tx = return $ ((reward + rTotal), (svTotal + sumA (stateValue ^-^ rewardToGo)), len + 1)
    end (rTotal, svTotal, len) = (liftIO . putStrLn $
                        "average reward: " <> (show $ round $ rTotal / len)
                        <> " average sv-error: " <> (show $ round $ svTotal / len) 
                        <> " nEpisodes : " <> (show . round $ len))
{-# INLINE minibatchStatistics #-}


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
