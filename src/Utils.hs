{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Utils where

import System.Random
import qualified Data.Vector.Sized as VS
import Control.Monad.State
import GHC.TypeLits
import ConCat.Misc
import ConCat.Deep
import GHC.Generics hiding (R)



type V = VS.Vector

type PType i h o = ((V h --+ V o) :*: (V h --+ V h) :*: (V i --+ V h)) R

-- | Create an arbitrary functor filled with different random values.
randF :: (Traversable f, Applicative f, Random a) => Int -> f a
randF = evalState (sequenceA $ pure $ state random) . mkStdGen
{-# INLINE randF #-}


gaussInit :: Num a => a -> a
gaussInit = (\x -> 2 * x - 1)
{-# INLINE gaussInit #-}


class (KnownNat n) => HasV a n where
  toV :: a -> V n R
  fromV :: V n R -> a


-- | Convert from degrees to radians
toRadians :: Floating a => a -> a
toRadians deg = deg*pi/180
