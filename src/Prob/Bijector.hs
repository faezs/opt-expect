{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bijector where

import Prelude hiding (id, (.), lookup)
import ConCat.Distribution
import GHC.Generics hiding (R)
import ConCat.Misc
import ConCat.Isomorphism
import ConCat.Interval
import ConCat.Incremental
import qualified Data.Map as Map
import Data.Maybe
import Data.Bifunctor
import ConCat.RAD
import ConCat.Free.VectorSpace
import Control.Monad.Trans.State.Lazy
import ConCat.Category hiding (first, second)
import System.Random

--type R = Double

--type LProb = Double

instance Bifunctor Dist where
  bimap f g (Dist d) = Dist $ undefined

instance Functor (Dist a)
instance Foldable (Dist a)
instance Traversable (Dist a)
instance Applicative (Dist a)


exactlyT :: Dist Bool Int
exactlyT = exactly testF

testF True = 10
testF False = -10
  
class Bijection k d pa d' pb where
  forward :: (d `k` d') -> (pa `k` pb) -> Dist d pa -> Dist d' pb
  inverse
    :: (d' `k` d) -> (pb `k` pa) -> Dist d' pb -> Dist d pa
  logDetJacobian
    -- log absolute value of the determinant
    -- of the matrix of all first-order partial derivatives
    -- of the inverse function
    :: (d' `k` d) -> (pb `k` pa) -> (Dist d' pb) -> R
    -- We use this to invert a transformation to compute one probability in terms of another
    -- geometrically, this is the volume of transformation and is used to scale the probability
    -- We can discard the sign because we're integration over probability densities which are all >= 0.

--data a :<-> b = Dist a b -> Dist a b

--laws d = inverse . forward $ d == id d


sample :: forall a b. (Ord b) => (a -> b) -> Dist a b -> a -> (b, R)
sample ab (Dist f) a = (\x -> (b, (fromMaybe @ R) 0 x)) <$> Map.lookup b $ f a
  where
    b = ab a
    r :: R
    r = br $ f a
    br :: Map.Map b R -> R
    br = (Map.! b)

sampleT :: Dist Bool Int -> Bool -> (Int, R)
sampleT = sample testF
--transformLogProb :: forall d pa pb. (Bijection d d pa pb) => Dist d pa -> Dist d pb -> R -> d -> R
--transformLogProb d d' log_prob sample = logDetJacobian (inverse @ d @ d @ pa @ pb) sample + log_prob

  -- where d' = ((forward @ d @ d @ a @ b) distA)


newtype Bj f g a b = Bj (Dist (f a) (g b)) deriving (Generic)

instance (Functor f, Functor g) => Bijection (Bj f g) (f a) (g b) a b where
  forward = undefined --(Bj d) 

jacobian :: forall a b c. a -> Dist a b -> Map.Map b R -> a
jacobian a d@(Dist f) = derR (f) a
  where
    -- we need the gradient of the function defined by b -> R
    -- over a finitely-represented domain,
    -- which can be converted to a generalized matrix.
    a' = undefined


randF :: (Traversable f, Applicative f, Random a) => Int -> f a
randF = evalState (sequenceA $ pure $ state random) . mkStdGen
{-# INLINE randF #-}

randD :: (Random b) => Int -> Dist a b
randD = randF
