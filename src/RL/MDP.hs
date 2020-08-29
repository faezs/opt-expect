{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RL.MDP where

import GHC.Generics hiding (R)

import Data.Monoid
import Control.Applicative
import Control.Monad.Bayes.Class
import Control.Monad.Trans.State.Strict hiding (get, modify', put, state)
import Control.Monad.State.Class
import Control.Monad.Trans.Class ()


import ConCat.RAD (gradR, andDerR, andGrad2R)
import ConCat.Misc

import Streamly
import qualified Streamly.Prelude as S

import Env.MonadEnv


-- $ A Markov process takes place in a probability monad,
-- $ and has s as its state
data MarkovProcess m s = MP {
  step :: s -> m s
}

-- $ Running a process is iterating monadically over the state
-- $ s -> m s . s -> m s . s -> m s
simulate :: forall t m s. (IsStream t, MonadAsync m)
  => MarkovProcess m s
  -> s
  -> t m s
simulate process s = S.iterateM (step process) (return s)


----------------Dealing With Rewards ----------------------------------
-- $ The reward defines what to accomplish and is a monoidal number
type Reward = Sum Double

 -- $ One step of a MarkovRewardProcess gives us the next state and the reward
-- $data MarkovRewardProcess m s = MRP { step' :: s -> m (s, Reward) } 
-- $ This is a WriterT Reward around m
type MarkovRewardProcess m s = MarkovProcess (StateT Reward m) s


-- $ This is how we inject the reward at each step 
reward :: (MonadState Reward m) => Double -> m ()
reward x =  modify' (<> Sum x)

--------------------- MDP ----------------------------------
-- $ an MDP takes a state and an action and returns the next state and reward 
data MarkovDecisionProcess m s a = MDP { act :: s -> a -> StateT Reward m s }


-- $ A function from state to action
type Policy s a = s -> a


-- $ Choose the action that maximizes our total expected reward
-- $                 This is equivalent to
-- $ Find the Policy that maximizes our total expected reward


-- $ Application of the policy to the Markov Decision Process returns a Markov Reward Process

-- $ MDP + Policy = MRP
apply :: forall m s a. Policy s a -> MarkovDecisionProcess m s a -> MarkovRewardProcess m s
apply policy (MDP{ act }) = MP { step = \ s -> act s (policy s) } 



data StochasticMDP m s a = SMDP { actS :: s -> m a -> StateT Reward m s }

type StochasticPolicy m s a = s -> m a

applyS :: forall m s a. StochasticPolicy m s a -> StochasticMDP m s a -> MarkovRewardProcess m s
applyS stocPol (SMDP { actS }) = MP { step = \ s -> actS s (stocPol s) }


data ParameterizedMDP p m s a = PMDP { actP :: s -> m a -> StateT Reward m s }

type ParameterizedPolicy p m s a = (p, p -> s -> m a)

applyP :: forall p m s a. ParameterizedPolicy p m s a -> StochasticMDP m s a -> MarkovRewardProcess m s
applyP (params, policy) (SMDP {actS}) = MP { step = \ s -> actS s (policy params s) }


-- EXAMPLE
{--
demandForecast :: MonadSample m => m Double
demandForecast = normal 100 50

store :: (Monad m) => m Double -> MarkovProcess m Double
store demand = MP {
  step = \inventory -> do
      let ordered = 3
      demanded <- demand
      let sold = min inventory demanded
      pure (inventory - sold + ordered)
}

storeMDP :: (Monad m) => m Double -> MarkovDecisionProcess m Double Double
storeMDP demand = MDP {
  act = \ inventory ordered -> do
      reward (-1 * inventory)
      reward (-4 * ordered)

      demanded <- lift demand
      let sold = min demanded inventory
          missed = demanded - sold
      reward (sold * 6 - missed * 12)
      pure (inventory - sold + ordered)
}


storeSMDP :: (Monad m) => m Double -> StochasticMDP m Double Double
storeSMDP demand = SMDP {
  actS = \ inventory orderer -> do
      ordered <- lift orderer
      reward (-1 * inventory)
      reward (-4 * ordered)

      demanded <- lift demand
      let sold = min demanded inventory
          missed = demanded - sold
      reward (sold * 6 - missed * 12)
      pure (inventory - sold + ordered)
}

a :: Policy Double Double
a = const 4
--}

runMDP :: MarkovDecisionProcess MonadEnv s a -> Policy s a -> MarkovRewardProcess MonadEnv s
runMDP = flip apply  

runSMDP :: StochasticMDP MonadEnv s a -> StochasticPolicy MonadEnv s a -> MarkovRewardProcess MonadEnv s
runSMDP = flip applyS

runPSMDP :: StochasticMDP MonadEnv s a -> ParameterizedPolicy p MonadEnv s a -> MarkovRewardProcess MonadEnv s
runPSMDP = flip applyP
