{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Policy where

import Control.Monad.Bayes.Class
import Utils

import ConCat.Misc
import ConCat.AltCat ()
import ConCat.Rebox ()
import ConCat.Additive

import qualified Data.Vector.Generic as VG
import Data.Key
import Data.Foldable

import Control.Comonad

import GHC.Generics hiding (R)
import GHC.TypeLits


{--
class (MonadSample m) => Policy m (p :: Nat -> Nat -> Nat -> *) s a r where
  sample :: (HasV s i, HasV a o, KnownNat h) => (p i h o -> s -> a) -> s -> m (a, r)
  logProb :: (HasV s i, HasV a o, KnownNat h) => a -> R
  policy :: p i h o -> s -> a
  value :: p i h o -> s -> r
--}


sampleCat :: (MonadSample m, VG.Vector f R, Foldable f, Enum a) => f R -> m a
sampleCat cs = toEnum <$> (categorical cs)
{-# INLINE sampleCat #-}

logProbCat :: (Indexable f) => f R -> Key f -> R
logProbCat v i = log $ index v i
{-# INLINE logProbCat#-}

sampleGaussian :: (MonadSample m, Applicative f, Traversable f) => f R -> f R -> m (f R)
sampleGaussian mu std = sequenceA $ normal <$> mu <*> std
{-# INLINE sampleGaussian #-}

logProbGaussian :: (Applicative f) => f R -> f R -> f R -> f R
logProbGaussian mu std sample = extract <$> (normalPdf <$> mu <*> std <*> sample)
{-# INLINE logProbGaussian #-}


gaussianPolicy ::
  forall m p a b.
  ( MonadSample m
  , Applicative a
  , Traversable a
  , Applicative b
  , Traversable b
  , Functor p)
  => (p R -> a R -> b R) -- fn from inputs to mu
  -> (p R -> a R -> b R) -- fn from inputs to std
  -> p R
  -> a R          -- inputs
  -> m (b R)      -- samples
gaussianPolicy muFn stdFn ps xs = (sampleGaussian @m @b) (muFn ps xs) (stdFn ps xs)
{-# INLINE gaussianPolicy #-}
