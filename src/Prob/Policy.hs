{-# LANGUAGE BangPatterns #-}
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
module Prob.Policy where

import Control.Monad.Bayes.Class
import Utils.Utils

import ConCat.Misc
import ConCat.AltCat ()
import ConCat.Rebox ()
import ConCat.Additive

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Sized as VS
import Data.Key
import Data.Foldable
import qualified Data.Functor.Rep as Rep

import Control.Comonad

import GHC.Generics hiding (R)
import GHC.TypeLits
import Control.Monad

{--
class (MonadSample m) => Policy m (p :: Nat -> Nat -> Nat -> *) s a r where
  sample :: (HasV s i, HasV a o, KnownNat h) => (p i h o -> s -> a) -> s -> m (a, r)
  logProb :: (HasV s i, HasV a o, KnownNat h) => a -> R
  policy :: p i h o -> s -> a
  value :: p i h o -> s -> r
--}

-- | Draw from a discrete distribution using a sequence of draws from Bernoulli.
fromPMF' :: MonadDistribution m => Int -> (Int -> Double) -> m Int
fromPMF' (!n) !p = f 0 1
  where
    f !i !r = do
      when (r < 0) $ error "fromPMF: total PMF above 1"
      let q = p i
      when (q < 0 || q > 1) $ error "fromPMF: invalid probability value"
      b <- bernoulli (q / r)
      if b then pure i else if (i + 1 >= n) then pure i else f (i + 1) (r - q)
{-# INLINE fromPMF' #-}

categorical' :: (MonadDistribution m, KnownNat n) => V n R -> m Int
categorical' (!ps) = fromPMF' (VS.length ps) $! (VS.unsafeIndex ps)
{-# INLINE categorical' #-}

sampleCat :: (MonadDistribution m, KnownNat n, Enum a) => V n R -> m a
sampleCat = \cs -> toEnum <$> (categorical' $! cs)
{-# INLINE sampleCat #-}

logProbCat :: (Indexable f) => f R -> Key f -> R
logProbCat v i = log $ index v i
{-# INLINE logProbCat#-}

sampleGaussian :: (MonadDistribution m, Applicative f, Traversable f) => f R -> f R -> m (f R)
sampleGaussian mu std = sequenceA $ normal <$> mu <*> std
{-# INLINE sampleGaussian #-}

logProbGaussian :: (Applicative f) => f R -> f R -> f R -> f R
logProbGaussian mu std sample = extract <$> (normalPdf <$> mu <*> std <*> sample)
{-# INLINE logProbGaussian #-}


gaussianPolicy ::
  forall m p a b.
  ( MonadDistribution m
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
