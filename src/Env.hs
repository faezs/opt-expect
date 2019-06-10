{-# LANGUAGE DatatypeContexts #-}

module Env () where

import GHC.TypeLits (KnownNat)
import Data.Array.Accelerate as A

type Done = Bool
type Reward = Double

data (A.Arrays a) => Observation a = Observation a
data (A.Arrays a) => Action a = Action a


{--
Observation and Action are meant to be polymorphic types over a vectorspace.
The State Space can be a vector or a matrix or a tensor of type t.
The Action Space can be a vector or a matrix or a tensor of type t.
I want these to be a wrapper around accelerate.

--}

class (Functor a) => Env a where
  init :: a
  step :: a -> Action -> (Observation, Reward, Done)
  computeReward :: Observation -> Reward
  reset :: Observation
  updateEnvWithAction :: a -> Action -> a


data Microgrid = Microgrid
  { nodes :: [Node]
  , gridVoltage :: 
  ,
  }

instance Env Microgrid where
  step (Microgrid nodes) action = getEnvObservation updateNodeStates nodes action
  computeReward (Microgrid nodes)
