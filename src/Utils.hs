{-# LANGUAGE MultiParamTypeClasses #-}
module Utils where

import System.Random
import qualified Data.Vector.Sized as VS
import Control.Monad.State
import GHC.TypeLits
import ConCat.Misc

type V = VS.Vector

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
