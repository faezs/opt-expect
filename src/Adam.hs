{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module Adam where

import Prelude hiding (zipWith)
import GHC.TypeLits
import GHC.Generics hiding (R)

import ConCat.Deep
import ConCat.RAD (gradR)

import Data.Key
import ConCat.AltCat (Additive1(..),(<+))
import ConCat.Rebox ()
import ConCat.Misc
import ConCat.Additive


type GradCon p s b = (Foldable b, Zip b, Functor p, Zip p, Additive1 p, Additive s, Num s, Floating s, Fractional s)

type Loss b s = (Foldable b, Zip b, Num s, Floating s, Fractional s) => b s -> b s -> s


adamStep :: forall s p a b.
     (GradCon p s b)
  => s -> s -> s -> Loss b s -> (p s -> a s -> b s) -> s -> a s :* b s -> Unop (p s :* (p s :* p s))
adamStep = \b1 b2 eta loss m gamma (xs, ys) (p, (mp, vp)) -> let
  ((m', v'), (m'', v'')) = adamAveraging b1 b2 mp vp g
  g = gradR (\p -> loss (m p xs) ys) p
  in
  (p ^-^ (zipWith (*) m'' (fmap (\x -> gamma / sqrt x + eta) v'')), (m', v'))
{-# INLINE adamStep #-}


adamAveraging :: (Functor p, Zip p, Additive s, Num s, Fractional s) => s -> s -> p s -> p s -> p s -> ((p s :* p s) :* (p s :* p s))
adamAveraging = \b1 b2 mp vp g -> let
  m'' = m' ^/ (1 - b1)
  v'' = v' ^/ (1 - b2)
    -- exponential moving average update step
    -- for the mean and variance of the parameter gradient
    -- with respect to the input output 
  m' = zipWith (+) (b1 *^ mp) ((1 - b1) *^ g)
  v' = zipWith (+) (b2 *^ vp) ((1 - b2) *^ ((^2) <$> g))
  in ((m', v'), (m'', v''))
{-# INLINE adamAveraging #-}

adamSteps :: forall s p a b f.
  ( GradCon p s b, Applicative p, Functor f, Foldable f)
  => Loss b s -> (p s -> a s -> b s) -> s -> f (a s :* b s) -> Unop (p s)
-- steps m gamma samples = compose (step m gamma <$> samples)
adamSteps = \loss m gamma samples p -> fst3 $ compose ((adamStep b1 b2 eta loss m gamma) <$> samples) (p, (m0, v0))
  where
    fst3 = \(a, (_, _)) -> a
    b1 = 0.9 :: s
    b2 = 0.999 :: s
    eta = 10e-8 :: s
    m0 = (pure zero) :: p s
    v0 = (pure zero) :: p s
{-# INLINE adamSteps #-}
