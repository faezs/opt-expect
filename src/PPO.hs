{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:showResiduals #-}
--{-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:trace #-}
--{-# OPTIONS_GHC -fsimpl-tick-factor=500 #-}

module PPO where

import Prelude hiding (zip, zipWith, length)
import Data.Foldable
import ConCat.Deep
import ConCat.Misc
import ConCat.Rebox  -- Necessary for reboxing rules to fire
import ConCat.AltCat (forkF, fork, fromIntegralC, sumAC, Additive1, (<+), additive1)  -- Necessary, but I've forgotten why.
import ConCat.RAD (gradR)
import ConCat.Additive ((^+^), Additive, sumA)
import ConCat.Sized

import qualified Data.Vector.Sized as VS
import Data.Maybe (fromJust)
import Data.Key
import qualified Data.Vector.Generic as VG

import GHC.Generics hiding (R)
import GHC.TypeLits
import Data.Finite

import Utils
import Control.Monad.Bayes.Class (MonadSample)
import Policy
import Env


softmax :: (Functor f, Functor g, Foldable g, Fractional s, Floating s, Additive s) => Unop (f (g s))
softmax = (normalize <$>) . ((fmap.fmap) (\a -> exp a))
{-# INLINE softmax #-}

valueFn :: (KnownNat i, KnownNat h) => PType i h 1 -> (V i --> V 1) R
valueFn = lr3'
{-# INLINE valueFn #-}

policy :: (KnownNat i, KnownNat h, KnownNat o) => PType i h o -> (V i --> V o) R
policy = lr3'  -- Defined in ConCat.Deep.
{-# INLINE policy #-}

catAgent :: forall m i h o s a. (MonadSample m, HasV s i, HasV a o, Enum a, KnownNat h) => PType i h o -> s -> m a
catAgent = \ps s -> toEnum <$> (sampleCat . (softmax . policy $ ps) . toV) s
{-# INLINE catAgent #-}

forkV :: forall i o. (KnownNat i, KnownNat o) => ((V i --> V o) R, (V i --> V o) R) -> (V i R -> (V o R, V o R)) 
forkV = fork
{-# INLINE forkV #-}

gaussAgent :: forall m i h o s a. (MonadSample m, HasV s i, HasV a o, KnownNat h) => PType i h o -> PType i h o -> s -> m a
gaussAgent = \mu std s -> fromV <$> (uncurry sampleGaussian) (forkV (policy mu, policy std) $ toV s)
{-# INLINE gaussAgent #-}

policyGradient :: forall i h o s a. (KnownNat i, KnownNat h, KnownNat o, HasV a o, HasV s i) => R -> Episode s a R -> PType i h o -> PType i h o
policyGradient = \lr episode params -> params ^-^ (lr *^ ((gradLogProbExp episode) params))
{-# INLINE policyGradient #-}


{----------------------------------- PPO-Clip ------------------------------------------}


valueFnLearn :: forall s a i h. (HasV s i, KnownNat h) => (PType i h 1 -> V i R -> V 1 R) -> Episode s a R -> PType i h 1 -> PType i h 1
valueFnLearn = \valueFn eps vParams -> steps valueFn 0.1 (trainingPairs eps) vParams
  where
    trainingPairs :: Episode s a R -> [(V i R, V 1 R)]
    trainingPairs = \Episode{..} -> zip ((\Transition{..} -> (toV s_t)) <$> trajectories) (VS.singleton <$> rewardToGo)
{-# INLINE valueFnLearn #-}

ppoUpdate :: forall i h o s a. (KnownNat i, KnownNat h, KnownNat o, HasV s i, HasV a o) => R -> Episode s a R -> PType i h o -> PType i h 1
ppoUpdate = undefined

{-------------------------------- Vanilla Policy Gradient -----------------------------}


-- Expectation over the grad log prob for all trajectories of an episode
gradLogProbExp :: (KnownNat i, KnownNat h, KnownNat o, HasV a o, HasV s i) => Episode s a R -> (Unop (PType i h o))
gradLogProbExp = \Episode{..} policyParams ->
  scaleV ((-1) / (fromIntegralC . length $ trajectories) :: R)
  (sumA $ (\(s, a, rtg) -> gradLogProb (rtg) s a policyParams)
    <$> (fmap (\(rtg, Transition{..}) ->
                  (toV s_t, toV a_t, rtg)) (zip (repeat reward) trajectories)))
{-# INLINE gradLogProbExp #-}


-- Gradient of the loss over one trajectory
-- episode_reward * logProb of the action given policy with params on the state stPPO
gradLogProb :: forall i h o. (KnownNat h, KnownNat i, KnownNat o) => R -> V i R -> V o R -> PType i h o -> PType i h o
gradLogProb = \epR st act params -> gradR (\ps -> epR * (((policy @i @h @o ps) st) <.> act)) params
{-# INLINE gradLogProb #-}
