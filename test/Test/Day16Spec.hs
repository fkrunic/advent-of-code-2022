module Test.Day16Spec (spec) where

import Test.Hspec

spec :: SpecWith ()
spec = 
  describe "Day 16 Tests" $ do
    it "Parsing" $ do
      1 `shouldBe` 1