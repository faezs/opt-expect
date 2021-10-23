{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RL.MDPLearn where

import ConCat.RAD (gradR)

import Control.Monad.Bayes.Class ()
import Control.Monad.Trans.State.Strict hiding (get, modify', put, state)
import Control.Monad.State.Class
import Control.Monad.Trans.Class () --(lift)

import Streamly
import qualified Streamly.Prelude as S
-- import qualified Streamly.Internal.Prelude as S
import Env.MonadEnv

import RL.MDP
{--
data StochasticMDP m s a = SMDP { actS :: s -> m a -> StateT Reward m s }

data PMDP p m s a = PMDP { actP
                           :: s
                           -> (p -> s -> m a)
                           -> MarkovDecisionProcess m s a
                           -> MarkovRewardProcess m s
                         , params :: p
                         }

type PMarkovRewardProcess m p s = MarkovProcess (StateT (p, Reward) m) s

applyP :: forall p m s a. PMDP p m s a ->  ParameterizedPolicy p m s a -> PMarkovRewardProcess m s a
applyP (PMDP {actP, params}) (Policy {policy, update}) = MP { step = \s -> actP s (policy params) update }


type StochasticPolicy m s a = s -> m a

applyS :: forall m s a. StochasticPolicy m s a -> StochasticMDP m s a -> MarkovRewardProcess m s
applyS stocPol (SMDP { actS }) = MP { step = \ s -> actS s (stocPol s) }


data ParameterizedPolicy p m s a = Policy {
    policy :: (p -> s -> m a)
  , update :: (p -> (s, a) -> (s, Reward) -> p)
}


policyGradient :: StochasticMDP m s a -> ParameterizedPolicy p m s a -> ParameterizedPolicy p m s a
policyGradient = undefined



pRun :: (MonadAsync m) => PMDP p m s a -> ParameterizedPolicy p m s a -> s -> SerialT m (Reward, s)
pRun mdp policy initState = S.runStateT 0 $ simulate (applyP mdp policy) initState

--}
