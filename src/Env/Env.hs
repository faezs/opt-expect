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

{-# OPTIONS_GHC -fplugin=Fusion.Plugin #-}

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
import Data.Maybe (fromJust, isJust)
import Data.Default
import Data.Proxy


import GHC.Generics hiding (R)
import GHC.TypeLits

import Utils hiding (R)
import MLUtils (HasLayers(..), showPart)
import ConCat.Deep


import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as S
import Streamly
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Unfold.Types as UF
import qualified Streamly.Internal.Data.Stream.StreamD.Type as STy
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

sampleIOE :: MonadEnv a -> IO a
sampleIOE = sampleIO . runMonadEnv


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

{--
totalR :: forall t m s a r. (IsStream t, EnvCon m s a r) => t m (Transition s a r) -> t m (Transition s a r)
totalR xs = S.postscan accumRw xs
  where
    --sumRw :: FL.Fold m (Transition s a r) r
    --sumRw = FL.Fold (\x y -> return $ (x + rw y)) (pure $ 0) (pure)
    --subRwByTotal :: r -> Transition s a r -> Transition s a r
    --subRwByTotal tot t1 = t1{rw = tot - rw t1}
    accumRw :: FL.Fold m (Transition s a r) (Transition s a r)
    accumRw = FL.Fold step (pure def) (pure)
      where
        step t0 t1 = return $ t1{rw = rw t0 + rw t1}
{-# INLINE totalR #-}
--}

advantageF :: forall t m s a r. (IsStream t, EnvCon m s a r) => (s -> r) -> r -> r -> t m (Transition s a r) -> t m (Transition s a r)
advantageF vf lambda gamma txs = S.postscan advFold $ S.zipWith (,) (S.enumerateFrom (0 :: Int)) txs
  where
    advFold :: FL.Fold m (Int, Transition s a r) (Transition s a r)
    advFold = FL.Fold accAdv (pure (def @(Transition s a r))) (pure)
    accAdv t0@Transition{advantage=advP, rw=rp} (i, t1@Transition{..}) =
      pure $ t1{ rw = rp + rw
               , advantage = advP + (lambda * gamma ^ i * oneStepTDResidual)
               , tdRes = oneStepTDResidual
               , stateValue = v_t
               }
      where
        v_t = vf s_t
        oneStepTDResidual = rw + (gamma * vf s_tn) - (v_t)
--{-# INLINE advantageF #-}

-- :: (Num r) => r -> r -> Transition s a r -> r
--oneStepTDResidual v_t gamma t@Transition{..} = rw + (gamma * vf s_tn) - (v_t)
--{-# INLINE oneStepTDResidual #-}

minibatch :: forall n t m s a r. (IsStream t, Monad m, KnownNat n) => t m (Transition s a r) -> t m (V n (Transition s a r))
minibatch trajectories = S.map (fromJust)
                         $ S.takeWhile isJust
                         $ (VS.fromList @n)
                         <$> S.chunksOf (fromInteger $ natVal (Proxy @n)) FL.toList trajectories
--{-# INLINE minibatch #-}

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
  episode = S.takeWhile (\tx -> not . done $ tx)
            $ (S.evalStateT initS (S.mapM (\_ -> stepFn agent) $ S.enumerateFromTo (1 :: Int) 1000))
  epWithAdvantage = (advantageF valueFn 0.99 0.2) episode 
  in epWithAdvantage
--{-# INLINE runEpisode #-}


unfoldEpisode ::
  forall m s a r.
  ( EnvCon m s a r
  , MonadSample m
  , MonadAsync m
  )
  => ((s -> m a) -> s -> m (Transition s a r))
  -> (s -> m a)
  -> s
  -> UF.Unfold m s (Transition s a r)
unfoldEpisode stepFn agent initS = UF.Unfold tn (stepFn agent)
  where
    tn :: (Transition s a r -> m (STy.Step (Transition s a r) (Transition s a r)))
    tn t@Transition{..} = return $ if not done then STy.Yield t t else STy.Stop    
{--
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
--}


shuffleSized :: forall m n a. (MonadIO m, KnownNat n) => V n a -> m (V n a)
shuffleSized v = (return . fromJust . VS.toSized @n) =<< (liftIO . shuffleM . VS.fromSized $ v)
--{-# INLINE shuffleSized #-}



data BatchStatistics = BatchStatistics
  { batchReward :: R
  , batchAdvantage :: R
  , batchSVerror :: R
  } deriving (Eq, Ord, Show)

instance Semigroup BatchStatistics where
  s1 <> s2 = BatchStatistics { batchReward = batchReward s1 + batchReward s2
                             , batchAdvantage = batchAdvantage s1 + batchAdvantage s2
                             , batchSVerror = batchSVerror s1 + batchSVerror s2
                             }

instance Monoid BatchStatistics where
  mempty = BatchStatistics 0 0 0

computeStats :: V n (Transition s a R) -> BatchStatistics
computeStats = foldl (\b@BatchStatistics{..} t@Transition{..} -> b{batchReward = batchReward + rw
                                                                  , batchAdvantage = batchAdvantage + advantage
                                                                  , batchSVerror = batchSVerror + stateValue
                                                                  }) mempty

minibatchStatistics :: forall n m s a. (MonadIO m, KnownNat n) => FL.Fold m (V n (Transition s a R)) ()
minibatchStatistics = FL.Fold (step) begin end
  where
    begin = pure mempty
    step :: BatchStatistics -> (V n (Transition s a R)) -> m BatchStatistics
    step b = return . computeStats
    end BatchStatistics{..} = (liftIO . putStrLn $
                        "average reward: " <> (show $ round $ batchReward / l)
                        `vsep` " average stateValue: " <> (show $ batchSVerror / l) 
                        `vsep` " average advantage estimate : " <> (show $ batchAdvantage / l)
                         `vsep` "# Episodes: " <> (show . round $ l))
      where
        vsep a b = a <> "\n" <> b
        l = (fromIntegral $ natVal (Proxy @n))
--{-# INLINE minibatchStatistics #-}


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
