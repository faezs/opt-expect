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

import GHC.Generics hiding (R)
import GHC.TypeLits
import Data.Finite

import CartPole
import Utils


type PType i h o = ((V h --+ V o) :*: (V h --+ V h) :*: (V i --+ V h)) R

--softmax :: (KnownNat i, KnownNat o) => R -> Unop ((V i --> V o) R)
softmax :: (Functor f, Functor g, Foldable g) => R -> f (g R) -> f (g R)
softmax = \temp m -> normalize <$> ((fmap.fmap) (\a -> exp a / temp) m)
{-# INLINE softmax #-}


policy :: (KnownNat i, KnownNat h, KnownNat o) => PType i h o -> (V i --> V o) R
policy = (softmax 0.95) . (affine @. lr2)  -- Defined in ConCat.Deep.
{-# INLINE policy #-}

valueFn :: PType 4 16 1 -> (V 4 --> V 1) R
valueFn = lr3'
{-# INLINE valueFn #-}

runEps :: (Functor f, Foldable f, Zip f, KnownNat o, KnownNat h, KnownNat i, HasV s i, HasV a o, Enum a) => (s -> (s -> a) -> f (Transition s a R)) -> Int -> s -> PType i h o -> [PType i h o]
runEps = \runEp n initS startPs -> take n $ iterate (\p -> (flip (pgUpdate 0.1) p) (rollOut runEp initS p)) startPs
{-# INLINE runEps #-}

agent :: (HasV s i, HasV a o, KnownNat h) => PType i h o -> s -> a
agent = \ps s -> fromV $ policy ps $ toV s
{-# INLINE agent #-}

rollOut :: (Functor f, Foldable f, HasV s i, HasV a o, KnownNat h) => (s -> (s -> a) -> f (Transition s a R)) -> s -> PType i h o -> f (Transition s a R)
rollOut = \runEp initS startPs -> runEp initS $ agent startPs
{-# INLINE rollOut #-}


pgUpdate :: forall f i h o a s. (Functor f, Foldable f, Zip f, KnownNat i, KnownNat h, KnownNat o, HasV a o, HasV s i, Enum a) => R -> f (Transition s a R) -> PType i h o -> PType i h o
pgUpdate = \lr transitions params -> params ^-^ (0.03 * 0.99 *^ compose (txLoss transitions) params)
{-# INLINE pgUpdate #-}

txLoss :: (Functor f, Foldable f, Zip f, KnownNat i, KnownNat h, KnownNat o, HasV a o, HasV s i) => f (Transition s a R) -> f (Unop (PType i h o))
txLoss = \transitions -> (\(s, a) -> pgGrad (epReward transitions) (fromIntegralC . length $ transitions) s a) <$> (fmap (\Transition{..} -> (toV s_t, toV a_t)) transitions)
{-# INLINE txLoss #-}

pgGrad :: forall i h o. (KnownNat h, KnownNat i, KnownNat o) => R -> R -> V i R -> V o R -> PType i h o -> PType i h o
pgGrad = \epR len st act params -> gradR (\ps -> (((\k-> epR * (-k)) <$> (policy @i @h @o ps) st) <.> act) / len) params
{-# INLINE pgGrad #-}

epReward :: forall f a s r. (Functor f, Foldable f) => f (Transition s a R) -> R
epReward = sumAC . (fmap rw)
{-# INLINE epReward #-}
