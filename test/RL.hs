module RL where

import Test.Hspec

spec = describe "test RL invariants" $ do
  it "id" $ do
    1 `shouldBe` 1
