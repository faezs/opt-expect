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
{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving, DeriveGeneric #-}

{-# OPTIONS_GHC -fplugin=Fusion.Plugin #-}

module Env.Env where

import Prelude hiding (length, zip, zipWith)

import ConCat.Misc
import ConCat.Additive

import ConCat.Category ((&&&))
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

import Utils.Utils hiding (R)
import Utils.MLUtils (HasLayers(..), showPart)
import ConCat.Deep


import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler.Strict

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as S
--import qualified Streamly.Internal.Prelude as S
import Streamly
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as UF
-- import qualified Streamly.Internal.Data.Unfold.Types as UF
import qualified Streamly.Internal.Data.Stream.StreamD.Type as STy
import qualified Data.Vector.Sized as VS


import Immutable.Shuffle
import Control.Newtype.Generics
import Control.Monad.Random.Class (MonadRandom)
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Catch



import Env.MonadEnv
import qualified Streamly.Internal.Data.Stream.IsStream.Enumeration as S


type EnvState m s r = (MonadDistribution m) => StateT s m r

type RLUpdate n i h o s a = V n (Transition s a R) -> Unop (PType i h o)

type RunEnv s a = ((s -> MonadEnv a)) -> EnvState MonadEnv s (Transition s a R)

type StochasticPolicy i h o s a = (PType i h o -> s -> MonadEnv a)

type ValueFn i h = (PType i h 1 -> V i R -> V 1 R)


newtype Reward a = Reward a
  deriving (Newtype, Generic, Show)
  deriving newtype (Eq, Ord, Fractional, Num)

newtype StateValue a = StateValue a
  deriving (Newtype, Generic, Show)
  deriving newtype (Eq, Ord, Fractional, Num)

newtype Advantage a = Advantage a
  deriving (Newtype, Generic, Show)
  deriving newtype (Eq, Ord, Fractional, Num)

avg :: (Num a, Fractional a) => Binop a
avg a b = a + b / 2

instance (Num a, Fractional a) => Semigroup (Reward a) where
  (<>) = avg

instance (Num a, Fractional a) => Semigroup (StateValue a) where
  (<>) = avg

instance (Num a, Fractional a) => Semigroup (Advantage a) where
  (<>) = avg

instance (Num a, Fractional a) => Monoid (Reward a) where
  mempty = Reward 0

instance (Num a, Fractional a) => Monoid (StateValue a) where
  mempty = StateValue 0

instance (Num a, Fractional a) => Monoid (Advantage a) where
  mempty = Advantage 0



data Transition s a r = Transition
  { s_t :: !s
  , s_tn :: !s
  , a_t :: !a
  , rw :: !r
  , done :: !Bool
  , advantage :: r
  , tdRes :: r
  , stateValueError :: r
  } deriving (Eq, Ord, Show, Generic)

instance (Default s, Default a, Num r) => Default (Transition s a r) where
  def = Transition def def def 0 False 0 0 0

type EnvCon m s a r = (MonadDistribution m, MonadAsync m, Default s, Default a, Num r, Fractional r, Show s, Show a, Show r)

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

type Discount r = Int -> r

type Lambda r = r
type Gamma r = r

glDiscount :: (Num r) => Lambda r -> Gamma r -> Discount r
glDiscount g l i = l * g ^ i

rewards :: (IsStream t, EnvCon m s a r) => t m (Transition s a r) -> t m (Reward r)
rewards = fmap (Reward . rw)

stateValues :: (IsStream t, EnvCon m s a r) => (s -> StateValue r) -> t m (Transition s a r) -> t m (StateValue r, StateValue r)
stateValues vf = fmap (\t -> (vf (s_t t), vf (s_tn t)))

advantages :: (IsStream t, EnvCon m s a r) => Lambda r -> Gamma r -> (s -> StateValue r) -> t m (Transition s a r) -> t m (Advantage r)
advantages l g vf s = S.postscanl' n mempty $ S.zipWith (,) (S.enumerateFrom (0 :: Int)) s
    where
      n a (i, t) = adv i g (glDiscount l g) (Reward $ rw t) (v t) a
        where
          v = (vf . s_t) &&& (vf . s_tn)

adv :: (Num r) => Int -> Gamma r -> Discount r -> Reward r -> StateValue r :* StateValue r -> Unop (Advantage r)
adv i g discount r (s, s') a = a + (Advantage (discount i) * tdDelta g r s s')

tdDelta :: (Num r) => Gamma r -> Reward r -> StateValue r -> StateValue r -> Advantage r
tdDelta g (Reward rw) (StateValue current) (StateValue future) = Advantage $ rw + current - (g * future)


advantageF :: forall t m s a r. (IsStream t, EnvCon m s a r) => (s -> r) -> r -> r -> t m (Transition s a r) -> t m (Transition s a r)
advantageF vf lambda gamma txs = S.postscan advFold $ S.zipWith (,) (S.enumerateFrom (0 :: Int)) txs
  where
    advFold :: FL.Fold m (Int, Transition s a r) (Transition s a r)
    advFold = FL.mkFoldM accAdv st pure
    st :: m (FL.Step (Transition s a r) (Transition s a r))
    st = pure (FL.Partial def)
    accAdv :: Transition s a r
           -> (Int, Transition s a r)
           -> m (FL.Step (Transition s a r) (Transition s a r))
    accAdv t0 (i, t1) =
      pure $ FL.Partial t1{ rw = rw'
                          , advantage = advantage t0 + (lambda * gamma ^ i * tdDel)
                          , tdRes = tdDel
                          , stateValueError = delV
                          }
      where
        rw' = rw t0 + rw t1
        delV = rw' - (vf . s_tn $ t0)
        tdDel = rw' + (vf . s_tn $ t0) - (gamma * (vf . s_tn $ t1))

--{-# INLINE advantageF #-}

-- :: (Num r) => r -> r -> Transition s a r -> r
--oneStepTDResidual v_t gamma t@Transition{..} = rw + (gamma * vf s_tn) - (v_t)
--{-# INLINE oneStepTDResidual #-}

minibatch :: forall n t m s a r. (IsStream t, Monad m, KnownNat n) => t m (Transition s a r) -> t m (V n (Transition s a r))
minibatch ts = S.map (fromJust)
                         $ S.takeWhile isJust
                         $ (VS.fromList @n)
                         <$> S.chunksOf (fromInteger $ natVal (Proxy @n)) FL.toList ts
{-# INLINE minibatch #-}

runEpisode ::
  forall m s a r.
  ( EnvCon m s a r
  , MonadDistribution m
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
            $ (S.evalStateT (pure initS) (S.mapM (\_ -> stepFn agent) $ S.enumerateFromTo (1 :: Int) 1000))
  epWithAdvantage = (advantageF valueFn 0.99 0.2) episode 
  in epWithAdvantage
  -- S.trace (liftIO . print . ("advantage,reward: " <>) . show . (advantage &&& rw))
--{-# INLINE runEpisode #-}


unfoldEpisode ::
  forall m s a r.
  ( EnvCon m s a r
  , MonadDistribution m
  , MonadAsync m
  )
  => ((s -> m a) -> s -> m (Transition s a r))
  -> (s -> m a)
  -> s
  -> UF.Unfold m s (Transition s a r)
unfoldEpisode stepFn agent initS = UF.mkUnfoldM tn (stepFn agent)
  where
    tn :: (Transition s a r -> m (STy.Step (Transition s a r) (Transition s a r)))
    tn t@Transition{..} = return $ if not done then STy.Yield t t else STy.Stop    
{--
runEpochs :: forall t m n s a i h o.
  ( HasV s i, HasV a o, KnownNat h
  , KnownNat n, Show s, Show a
  , IsStream t, MonadDistribution m, MonadAsync m
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
  { batchReward :: Reward R
  , batchAdvantage :: Advantage R
  , batchSVerror :: StateValue R
  } deriving (Eq, Ord, Show)

instance Semigroup BatchStatistics where
  s1 <> s2 = BatchStatistics { batchReward = batchReward s1 <> batchReward s2
                             , batchAdvantage = batchAdvantage s1 <> batchAdvantage s2
                             , batchSVerror = batchSVerror s1 <> batchSVerror s2
                             }

instance Monoid BatchStatistics where
  mempty = BatchStatistics mempty mempty mempty

computeStats :: V n (Transition s a R) -> BatchStatistics
computeStats = foldl (\b@BatchStatistics{..} t@Transition{..} -> b{batchReward = batchReward <> Reward rw
                                                                  , batchAdvantage = batchAdvantage <> Advantage advantage
                                                                  , batchSVerror = batchSVerror <> StateValue stateValueError
                                                                  }) mempty
{-# INLINE computeStats #-}


minibatchStatistics :: forall n m s a. (MonadIO m, KnownNat n) => FL.Fold m (V n (Transition s a R)) ()
minibatchStatistics = FL.mapM (end) $ FL.foldlM' step begin
  where
    begin = pure mempty
    step :: BatchStatistics -> (V n (Transition s a R)) -> m BatchStatistics
    step b = pure . computeStats
    end b = (liftIO . putStrLn $ show b `vsep` "# Episodes: " <> (show . round $ l))
      where
        vsep a b = a <> "\n" <> b
        l = (fromIntegral $ natVal (Proxy @n))
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
