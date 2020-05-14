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
import ConCat.AltCat (fromIntegralC, sumAC, Additive1, (<+), additive1)  -- Necessary, but I've forgotten why.
import ConCat.RAD (gradR)
import ConCat.Additive
import ConCat.Sized

import qualified Data.Vector.Sized as VS
import Data.Maybe (fromJust)
import Data.Key
import qualified Data.Vector.Generic as VG

import GHC.Generics hiding (R)
import GHC.TypeLits
import Data.Finite

import CartPole
import Utils
import Control.Monad.Bayes.Class
import Policy


type PType i h o = ((V h --+ V o) :*: (V h --+ V h) :*: (V h --+ V h) :*: (V i --+ V h)) R

--softmax :: (KnownNat i, KnownNat o) => R -> Unop ((V i --> V o) R)
softmax :: (Functor f, Functor g, Foldable g, Fractional s, Floating s, Additive s) => Unop (f (g s))
softmax = (normalize <$>) . ((fmap.fmap) (\a -> exp a))
{-# INLINE softmax #-}

policy :: (KnownNat i, KnownNat h, KnownNat o) => PType i h o -> (V i --> V o) R
policy = softmax . (affine @. lr3')  -- Defined in ConCat.Deep.
{-# INLINE policy #-}

catAgent :: (MonadSample m, HasV s i, HasV a o, Enum a, KnownNat h) => PType i h o -> s -> m a
catAgent = \ps s -> toEnum <$> (sampleCat . VS.fromSized . (policy ps) . toV) s
{-# INLINE catAgent #-}

pgUpdate :: forall f i h o a s. (Functor f, Foldable f, Zip f, KnownNat i, KnownNat h, KnownNat o, HasV a o, HasV s i, Enum a) => R -> f (Transition s a R) -> PType i h o -> PType i h o
pgUpdate = \lr transitions params -> params ^-^ (lr *^ compose (txLoss transitions) params)
{-# INLINE pgUpdate #-}

txLoss :: (Functor f, Foldable f, Zip f, KnownNat i, KnownNat h, KnownNat o, HasV a o, HasV s i) => f (Transition s a R) -> f (Unop (PType i h o))
txLoss = \transitions -> (\(s, a) -> pgGrad (epReward transitions) (fromIntegralC . length $ transitions) s a) <$> (fmap (\Transition{..} -> (toV s_t, toV a_t)) transitions)
{-# INLINE txLoss #-}

expectation :: (Foldable f, Functor f, Fractional a, Additive a) => f a -> a
expectation f = (sumA f) / (fromIntegralC . length $ f)
{-# INLINE expectation #-}


pgGrad :: forall i h o. (KnownNat h, KnownNat i, KnownNat o) => R -> R -> V i R -> V o R -> PType i h o -> PType i h o
pgGrad = \epR len st act params -> gradR (\ps -> (((\k-> epR * (-k)) <$> (policy @i @h @o ps) st) <.> act) / len) params
{-# INLINE pgGrad #-}

epReward :: forall f a s r. (Functor f, Foldable f) => f (Transition s a R) -> R
epReward = sumAC . (fmap rw)
{-# INLINE epReward #-}
