{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:showResiduals #-}

module PPO where

import ConCat.Deep
import ConCat.Misc     (R)
import ConCat.Rebox    ()  -- Necessary for reboxing rules to fire
import ConCat.AltCat   ()  -- Necessary, but I've forgotten why.
import ConCat.RAD (gradR)

import qualified Data.Vector.Sized as VS
import Data.Maybe (fromJust)

import GHC.Generics hiding (R)
import GHC.TypeLits

import CartPole
import Utils


type PType i h o = ((V h --+ V o) :*: (V h --+ V h) :*: (V i --+ V h)) R

policy :: (KnownNat i, KnownNat h, KnownNat o) => PType i h o -> (V i --> V o) R
policy = lr3'  -- Defined in ConCat.Deep.
{-# INLINE policy #-}

valueFn :: PType 4 16 1 -> (V 4 --> V 1) R
valueFn = lr3'
{-# INLINE valueFn #-}

runEps :: (KnownNat o, KnownNat h, KnownNat i, HasV s i, HasV a o) => (s -> (s -> a) -> [Transition s a R]) -> Int -> s -> PType i h o -> [PType i h o]
runEps = \runEp n initS startPs -> take n $ iterate (pgUpdate runEp initS) startPs
{-# INLINE runEps #-}

agent :: (HasV s i, HasV a o, KnownNat h) => PType i h o -> s -> a
agent = \ps s -> fromV $ policy ps $ toV s
{-# INLINE agent #-}

rollOut :: (HasV s i, HasV a o, KnownNat h) => (s -> (s -> a) -> [Transition s a R]) -> s -> PType i h o -> [Transition s a R]
rollOut = \runEp initS startPs -> runEp initS $ agent startPs
{-# INLINE rollOut #-}


pgUpdate :: (KnownNat i, KnownNat h, KnownNat o, HasV a o, HasV s i) => (s -> (s -> a) -> [Transition s a R]) -> s -> PType i h o -> PType i h o
pgUpdate = \runEp initS startPs -> foldl (\ps Transition{rw, s_t} -> policyGrad 0.1 0.99 rw ps policy (toV s_t)) startPs
                 $ rollOut runEp initS startPs
{-# INLINE pgUpdate #-}


policyGrad :: (KnownNat i, KnownNat h, KnownNat o)
  => R -- learning rate
  -> R -- discount
  -> R -- reward
  -> PType i h o
  -> (PType i h o -> V i R -> V o R)
  -> V i R
  -> PType i h o
policyGrad = \alpha gamma r ps pi st -> ps ^-^  (alpha * gamma * r *^ (gradR (fmap log (flip pi) st)) ps)
{-# INLINE policyGrad #-}
