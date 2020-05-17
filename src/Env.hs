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
import Control.Monad.Bayes.Class

import Data.Key
import Data.Foldable
import Data.Traversable

import GHC.Generics hiding (R)
import GHC.TypeLits

import Utils

import qualified Streamly.Prelude as S
import Streamly

import Control.Monad.Bayes.Sampler

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
  } deriving (Eq, Ord, Show, Generic)

instance Num r => Semigroup (Episode s a r) where
  a <> b = Episode
           { timesteps = timesteps a + timesteps b
           , reward = reward a + reward b
           , trajectories = trajectories a <> trajectories b
           , rewardToGo = rewardToGo a <> rewardToGo b
           }

instance Num r => Monoid (Episode s a r) where
  mempty = Episode 0 0 [] []

runEpisode ::
  forall m s a r.
  ( Additive r,
    MonadSample m,
    Num r
  )
  => ((s -> m a) -> EnvState m s (Transition s a r))
  -> (s -> m a)
  -> s
  -> m (Episode s a r)
runEpisode stepFn agent initS = do
  txs <- (takeWhileInclusive (\tx -> not . done $ tx))
           <$> (evalStateT (mapM (\_ -> stepFn agent) [1..200]) $ initS)
  return $ Episode
    { timesteps = length txs
    , reward = sumA (fmap rw txs)
    , trajectories = txs
    , rewardToGo = rTG txs 
    }
  where
    rTG xs = (\r -> totalR - r)
                   <$> (scanl (\rAtT Transition{rw} -> rAtT + rw) 0 xs)
      where
        totalR = sumA (fmap rw xs)
    takeWhileInclusive _ [] = []
    takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs else []
{-# INLINE runEpisode #-}



agentEpisode :: forall m p s a r i h o. (MonadSample m, HasV s i, HasV a o, KnownNat h)
       => (Episode s a r -> p i h o -> p i h o)       -- Learner
       -> (p i h o -> s -> m a)                       -- Agent
       -> (s -> (s -> m a) -> m (Episode s a r))      -- Environment
       -> Int                                         -- number of episodes
       -> s                                           -- initial state of the environment
       -> p i h o                                     -- initial parameters with an input size i, output size o
       -> m (p i h o)                                 -- list of parameters for the run
agentEpisode = \learner agent runEp n initS startPs ->
  (liftM . flip learner $ startPs) $ rollOut agent runEp initS startPs
{-# INLINE agentEpisode #-}


rollOut ::
  (MonadSample m, HasV s i, HasV a o, KnownNat h)
  => (p i h o -> s -> m a)
  -> (s -> (s -> m a) -> m (Episode s a r))
  -> s
  -> p i h o
  -> m (Episode s a r)
rollOut = \agent runEp initS startPs -> runEp initS (agent startPs)
{-# INLINE rollOut #-}
