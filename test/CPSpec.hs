module CPSpec (spec) where

import Test.Hspec
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Test.QuickCheck

import Env.CartPole

spec :: Spec
spec = do
  describe "" $ do
    it "nothing" $ do
      1 `shouldBe` 1
