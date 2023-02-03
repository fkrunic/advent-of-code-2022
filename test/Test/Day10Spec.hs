module Test.Day10Spec (spec) where

import Day10
import Test.Hspec

spec :: SpecWith ()
spec =
  describe "Day 10" $ do
    it "Part 1" $ do
      part1Solution puzzleInput `shouldBe` 13140