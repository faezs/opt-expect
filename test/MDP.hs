module MDP where

import Test.Hspec

spec = describe "test MDP invariants" $ do
  it "id" $ do
    1 `shouldBe` 1
