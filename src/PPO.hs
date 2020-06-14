{-# LANGUAGE AllowAmbiguousTypes #-}
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
import Data.Proxy
import Data.Finite

import Utils hiding (R)
import Control.Monad.Bayes.Class (MonadSample)
import Policy
import Env
import Adam

import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL

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

forkV :: forall i o o'. (KnownNat i, KnownNat o, KnownNat o') => ((V i --> V o) R, (V i --> V o') R) -> (V i R -> (V o R, V o' R)) 
forkV = fork
{-# INLINE forkV #-}

gaussAgent :: forall m i h o s a. (MonadSample m, HasV s i, HasV a o, KnownNat h) => PType i h o -> PType i h o -> s -> m a
gaussAgent = \mu std s -> fromV <$> (uncurry sampleGaussian) (forkV (policy mu, policy std) $ toV s)
{-# INLINE gaussAgent #-}


{----------------------------------- PPO-Clip ------------------------------------------}


type RLCon s a i h o =  (KnownNat i, KnownNat h, KnownNat o, HasV s i, HasV a o)
type KnownNat3 a b c = (KnownNat a, KnownNat b, KnownNat c)

valueFnLearn :: forall n s a i h. (HasV s i, KnownNat h, KnownNat n) => (PType i h 1 -> V i R -> V 1 R) -> R -> V n (Transition s a R) -> PType i h 1 -> PType i h 1
valueFnLearn = \valueFn gamma eps vParams -> (steps valueFn gamma (trainingPairs eps)) vParams
  where
    trainingPairs :: V n (Transition s a R) -> V n (V i R, V 1 R)
    trainingPairs = \trajectories -> ((\Transition{..} -> (toV s_t, VS.singleton rw)) <$> trajectories)
{-# INLINE valueFnLearn #-}


type RLFold n m s a r i h o = (RLCon s a i h o, KnownNat n, MonadAsync m)

ppoUpdate  :: forall n t m i h o s a.
  (RLCon s a i h o, KnownNat n, MonadAsync m, IsStream t)
  => R
  -> R
  -> t m (Transition s a R)
  -> PType i h o
  -> m (PType i h o)
ppoUpdate lr eta trajectories pi = do
  pis <- S.fold @m FL.last
         $ S.postscan (ppoBatch @n lr eta pi)
         $ adapt $ minibatch @n trajectories
  let (piOld, piNew) = fromJust $ pis
  return piNew
{-# INLINE ppoUpdate #-}

ppoBatch :: forall n m s a r i h o. RLFold n m s a r i h o => R -> R -> PType i h o -> FL.Fold m (V n (Transition s a R)) (PType i h o, PType i h o)
ppoBatch lr eta pi = FL.Fold step begin end
  where
    begin :: m (PType i h o, PType i h o)
    begin = pure $ (pi, pi)
    step :: (PType i h o, PType i h o) -> V n (Transition s a R) -> m (PType i h o, PType i h o)
    step (piOld, piNew) tx = pure $ (piNew, piNew ^+^ (lr *^ ppoGrad eta tx piOld piNew))
    end :: (PType i h o, PType i h o) -> m (PType i h o, PType i h o)
    end v = (return @m) v
{-# INLINE ppoBatch #-}

ppoGrad :: forall n i h o s a. (RLCon s a i h o, KnownNat n) => R -> V n (Transition s a R) -> PType i h o -> Unop (PType i h o)
ppoGrad = \eta trajectories piOld pi -> gradR (\p -> (sumA $ ((\tx -> (ppoLoss eta tx p pi)) <$> (preprocTx @s @a @i @o <$> trajectories)))) piOld
{-# INLINE ppoGrad #-}

ppoLoss :: forall i h o s a. (KnownNat3 i h o) => R -> (V i R, V o R, R) -> PType i h o -> PType i h o -> R
ppoLoss eta = \ (state, action, advantage) thetaOld theta -> min ((policyRatio theta thetaOld state action) * advantage) (g eta advantage)
{-# INLINE ppoLoss #-}

policyRatio :: (KnownNat3 i h o) => PType i h o -> PType i h o -> V i R -> V o R -> R
policyRatio = \pi pi' s a -> (logProb pi s a / logProb pi' s a)
{-# INLINE policyRatio #-}

g :: R -> R -> R
g = \eta adv -> if (adv >= 0) then (1 + eta) * adv else (1 - eta) * adv
{-# INLINE g #-}

preprocTx :: forall s a i o. (HasV s i, HasV a o) => (Transition s a R) -> (V i R, V o R, R)
preprocTx = (\Transition{..} -> (toV s_t, toV a_t, advantage))
{-# INLINE preprocTx #-}


{-------------------------------- Vanilla Policy Gradient -----------------------------}


policyGradient :: forall n i h o s a p. (KnownNat n, HasV s i, HasV a o, KnownNat h) => R -> V n (Transition s a R) -> (PType i h o -> s -> a) ->  Unop (PType i h o)
policyGradient = \lr trajectories pol params -> params ^+^ (lr *^ ((gradLogProbExp trajectories) params))
{-# INLINE policyGradient #-}

type ParamCon p = (Functor p, Zip p, Additive1 p)

-- Expectation over the grad log prob for all trajectories of an episode
gradLogProbExp :: forall n i h o s a. (KnownNat n, HasV s i, HasV a o, KnownNat h) => V n (Transition s a R) -> (Unop (PType i h o))
gradLogProbExp = \trajectories policyParams -> expectation $ (\(Transition{..}) -> gradLogProb advantage (toV s_t) (toV a_t) policyParams) <$> trajectories
{-# INLINE gradLogProbExp #-}


-- The log probablitity is multiplied by the log prob of the action given the state to avoid threading it though
-- into the expectation, which is just a sum 
gradLogProb :: forall i h o. (KnownNat h, KnownNat i, KnownNat o) => R -> V i R -> V o R -> PType i h o -> PType i h o
gradLogProb = \genAdv st act params -> (gradR (\ps -> genAdv * (logProb ps st act)) params) -- (genAdv * (logProb params st act)) *^ params --
{-# INLINE gradLogProb #-}

logProb :: forall i h o. (KnownNat h, KnownNat i, KnownNat o) => PType i h o -> V i R -> V o R -> R 
logProb = log . prob
{-# INLINE logProb#-}

prob :: forall i h o. (KnownNat h, KnownNat i, KnownNat o) => PType i h o -> V i R -> V o R -> R 
prob = \ps st act -> (((policy @i @h @o ps) st) <.> act)
{-# INLINE prob#-}


expectation :: (Zip f, Functor f, Foldable f, Additive a, Num a, Functor g, Additive (g a), Fractional a) => f (g a) -> g a
expectation fs = (sumA $ fs) ^/ (fromIntegral . length $ fs)
{-# INLINE expectation #-}
