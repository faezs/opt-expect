module DMonadicFn where

import ConCat.RAD


import Test.Hspec
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Test.QuickCheck


spec :: Spec
spec = do
  describe "Can we differentiate a monadic function?" $ do
    it "how do we differentiate a normal function? Lets try a 1-variable zinger " $ do
      let
        g :: (Num b) => (a -> b) -> a -> a
        g = gradR

        dx = let
          x k = fromIntegral k + 2
          in gradR x 10
      dx `shouldBe` 1
