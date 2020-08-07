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
import Control.Monad.Trans.State hiding (get, modify', put)
import Control.Monad.State.Class
import Control.Monad.Trans.Class


import ConCat.RAD (gradR, andDerR, andGrad2R)
import ConCat.Misc

import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Data.Fold as FL

import Algebra.Graph.Class
import qualified Algebra.Graph.Labelled as LG (Graph)
import qualified Algebra.Graph as G

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
simulate process state = S.iterateM (step process) (return state)


----------------Dealing With Rewards ----------------------------------
-- $ The reward defines what to accomplish and is a monoidal number
type Reward = Sum Double

-- $ One step of a MarkovRewardProcess gives us the next state and the reward
-- $data MarkovRewardProcess m s = MRP { step' :: s -> m (s, Reward) } 
-- $ This is a WriterT Reward around m
type MarkovRewardProcess m s = MarkovProcess (StateT Reward m) s


type StatefulMP ctx m s = MarkovProcess (StateT (ctx, Reward) m) s

-- $ This is how we inject the reward at each step 
reward :: (MonadState Reward m) => Double -> m ()
reward x =  modify' (<> Sum x)

--------------------- MDP ----------------------------------
-- $ an MDP takes a state and an action and returns the next state and reward 
data MarkovDecisionProcess m s a = MDP { act :: s -> a -> StateT Reward m s }

-- $ A function from state to action
type Policy s a = s -> a

type ParameterizedPolicy' p s a b = (p b -> s -> a)

type PolicyGradient p b = (p b -> p b)

--policyGradient :: (Functor p, Num b) => s -> ParameterizedPolicy' p s a b -> Unop (p b)
--policyGradient s p p' = gradR (\p' -> getReward $ p p' s) p'


-- $ Choose the action that maximizes our total expected reward
-- $                 This is equivalent to
-- $ Find the Policy that maximizes our total expected reward

-- $ Application of the policy to the Markov Decision Process returns a Markov Reward Process

-- $ MDP + Policy = MRP
apply :: forall m s a. Policy s a -> MarkovDecisionProcess m s a -> MarkovRewardProcess m s
apply policy (MDP{ act }) = MP { step = \ s -> act s (policy s) } 



applyS = undefined

--totalReward :: forall m s. (MonadAsync m, MonadSample m, Num s) => s -> MarkovRewardProcess m s -> m r
--totalReward initState rewardProcess = S.postscan FL.sum 
--  where
--sim :: IsStream t => s -> t (WriterT Reward m) s
--sim = simulate rewardProcess

--policyGradient forall m s. (MonadAsync m, MonadSample m, Num s) => s -> MarkovRewardProcess m s -> Policy s a ->  

data P' p s a = P' p s a

--pg :: forall m s a. (MonadAsync m, MonadSample m, Num s) => s -> MarkovDecisionProcess m s a -> Policy s a -> m (Policy s a)
--pg initState mdp policy = (flip gradR) policy rw
--  where
rw :: MarkovDecisionProcess m s a -> Policy s a -> m Reward
rw mdp p = undefined $ apply p mdp -- totalReward initState

g :: forall a s. (Num s) => (a -> s) -> a -> a
g f = gradR f



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

storeMDP :: (Monad m) => m Double -> MarkovDecisionProcess m Double Double -- MarkovDecisionProcess m s a
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


a :: Policy Double Double
a = const 4

runMDP :: MarkovDecisionProcess MonadEnv s a -> Policy s a -> MarkovRewardProcess MonadEnv s
runMDP = flip apply  

--}
