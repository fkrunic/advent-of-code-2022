module Test.Day17Spec where

import Test.Hspec

spec :: SpecWith ()
spec = do
  describe "Day 17 Tests" $ do
    it "Simple Test" $ do
      1 `shouldBe` (0 + 1)