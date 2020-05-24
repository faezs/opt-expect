{-# LANGUAGE ConstraintKinds #-}
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
import ConCat.GradientDescent (maximize)


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
import Adam


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



{----------------------------------- PPO-Clip ------------------------------------------}


type RLCon s a i h o =  (KnownNat i, KnownNat h, KnownNat o, HasV s i, HasV a o)

type AdvantageFn s a i h = (RLCon s a i h 1) => (PType i h 1 -> s -> a -> R)

type Advantage = R

type KnownNat3 a b c = (KnownNat a, KnownNat b, KnownNat c)

valueFnLearn :: forall s a i h. (HasV s i, KnownNat h) => (PType i h 1 -> V i R -> V 1 R) -> Episode s a R -> PType i h 1 -> PType i h 1
valueFnLearn = \valueFn eps vParams -> steps valueFn 0.1 (trainingPairs eps) vParams
  where
    trainingPairs :: Episode s a R -> [(V i R, V 1 R)]
    trainingPairs = \Episode{..} -> zip ((\Transition{..} -> (toV s_t)) <$> trajectories) (VS.singleton <$> rewardToGo)
{-# INLINE valueFnLearn #-}

ppoUpdate :: forall i h o s a. (RLCon s a i h o) => R -> R -> Episode s a R -> Unop (PType i h o)
ppoUpdate lr eta Episode{..} pi = expectation $ (\tx -> gradR (\pi' -> ppoLoss eta tx 1 pi' pi') pi) <$> trajectories
{-# INLINE ppoUpdate #-}

ppoLoss :: forall i h o s a. (RLCon s a i h o) => R -> Transition s a R -> Advantage -> PType i h o -> PType i h o -> R
ppoLoss eta Transition{..} adv thetaK theta = min (policyRatio theta thetaK (toV s_t) (toV a_t) * adv) (g eta adv)
{-# INLINE ppoLoss #-}

policyRatio :: (KnownNat3 i h o) => PType i h o -> PType i h o -> V i R -> V o R -> R
policyRatio pi pi' s a = (logProb pi s a / logProb pi' s a)
{-# INLINE policyRatio #-}

g :: R -> R -> R
g eta adv
  | adv >= 0 = (1 + eta) * adv
  | otherwise = (1 - eta) * adv
{-# INLINE g #-}

--klDivergence :: (Functor p, Foldable p, Zip p, Additive (p k), Num k) => p k -> p k -> k
--klDivergence p p' = fmap 
--{-# INLINE klDivergence #-}
{-------------------------------- Vanilla Policy Gradient -----------------------------}


--gradAscent :: (Functor p, Foldable p, Zip p, Additive (p k), Num k) => (p k) -> k -> Episode s a k -> (Episode s a k -> p k -> p k) -> p k
--gradAscent = \params lr episode loss ->  params ^+^ (lr *^ (loss episode params))

policyGradient :: forall i h o s a. (KnownNat i, KnownNat h, KnownNat o, HasV a o, HasV s i) => R -> Episode s a R -> Unop (PType i h o)
policyGradient = \lr episode params -> params ^-^ (lr *^ ((gradLogProbExp episode) params))
{-# INLINE policyGradient #-}


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
gradLogProb = \epR st act params -> gradR (\ps -> epR * (logProb ps st act)) params
{-# INLINE gradLogProb #-}

logProb :: forall i h o. (KnownNat h, KnownNat i, KnownNat o) => PType i h o -> V i R -> V o R -> R 
logProb = \ps st act -> (((policy @i @h @o ps) st) <.> act)
{-# INLINE logProb#-}


expectation :: (Functor f, Foldable f, Additive a, Num a, Functor g, Additive (g a), Fractional a) => f (g a) -> g a
expectation fs = (sumA fs) ^/ (fromIntegral $ length fs)
{-# INLINE expectation #-}
