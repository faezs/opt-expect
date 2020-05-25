{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module AdamSpec where

import Prelude hiding (zip, unzip, zipWith)
import Test.Hspec
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Test.QuickCheck.Utils
import Test.QuickCheck

import Adam
import ConCat.Deep
import Utils
import ConCat.Misc
import GHC.Generics hiding (R)
import ConCat.Additive
import qualified Data.Vector.Sized as VS
import Data.Maybe (fromJust)
import Data.Key

type Net = ((V 4 --+ V 1) :*: (V 4 --+ V 4) :*: (V 2 --+ V 4)) R

withinEta eta l = do
  (l <= abs eta) `shouldBe` True

spec :: Spec
spec = do
  describe " optimizer should find zeroes for the given functions " $ do
    it " ackley function " $ do
      let
        ackley :: R -> R -> R
        ackley x y = (- 20) * (exp (-0.2 * (sqrt (0.5 * (x^2 + y^2)))))
                     - (exp (0.5 * (cos ((2 * pi * x) + (2 * pi * y))))) + exp 1 + 20
      xs <- gens 500 $ (arbitrarySatisfying (\(x, y) -> -5 <= x && y <= 5) :: (Gen (R, R)))
      ys <- return $ (uncurry ackley) <$> xs
      let (l', p') = (f xs ys)
      print l'
      print p'
      withinEta 1e-1 l'

f xs ys = (loss', p ^-^ ps)
  where
    m = lr3 :: Net -> ((V 2 --> V 1) R)
    ps = (\x -> x - 0.5 / x + 5) <$> randF 5 :: Net
    td :: [(V 2 R, V 1 R)]
    td = zip (VS.fromTuple <$> xs) $ (VS.singleton <$> ys)
    (x', y') = unzip td
    p = last $ take 100 $ iterate (\p -> adamSteps distSqr m 0.001 td p) ps
    loss' = (sumA $ distSqr <$> (m p <$> x') <*> y') / (fromIntegral $ length y')
{-# INLINE f #-}
