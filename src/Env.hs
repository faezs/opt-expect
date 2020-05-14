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

type EnvState s r = State s r

data Transition s a r = Transition
  { s_t :: s
  , s_tn :: s
  , a_t :: a
  , rw :: r
  , done :: Bool
  } deriving (Eq, Ord, Show)



data Episode f s a r = Episode
  { timesteps :: Int
  , reward :: r
  , trajectories :: f (Transition s a r)
  } deriving (Generic)


runEpisode ::
  ( MonadSample m
  , Additive r
  , Applicative f
  , Foldable f
  , Traversable f)
  => (s -> (s -> m a) -> m (Transition s a r))
  -> (s -> m a)
  -> s
  -> m (Episode f s a r)
runEpisode env agent initS = Episode
                             { timesteps = length txs
                             , reward = sumA (S.map rw txs)
                             , trajectories = txs }
  where
    txs :: m (f (Transition s a r))
    txs = S.takeWhile (\Transition{done} -> done) $ S.iterateM (env initS agent)
    -- (takeWhileInclusive (\Transition{done} -> not done)) $ 
    takeWhileInclusive _ [] = []
    takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs else []


runEps :: (Functor f, Foldable f, Zip f,
           KnownNat o, KnownNat h, KnownNat i,
           HasV s i, HasV a o, Enum a, IsStream t)
       => (s -> (s -> m a) -> t m (Transition s a R)) -- Environment
       -> Int -- number of episodes
       -> s -- initial state of the environment
       -> p i h o -- parameters with an input size i, output size o
       -> t m (p i h o) -- list of parameters for the run
runEps = \learner runEp n initS startPs -> S.take n <$> S.iterateM (\p -> learner (rollOut runEp initS p)) startPs
-- (flip (pgUpdate 0.1) p)
{-# INLINE runEps #-}


rollOut ::
  (MonadSample m, Functor f, Foldable f, HasV s i, HasV a o, KnownNat h)
  => (p i h o -> s -> m a)
  -> (s -> (s -> m a) -> m (f (Transition s a R)))
  -> s
  -> p i h o
  -> m (f (Transition s a R))
rollOut = \agent runEp initS startPs -> runEp initS (agent startPs)
{-# INLINE rollOut #-}
