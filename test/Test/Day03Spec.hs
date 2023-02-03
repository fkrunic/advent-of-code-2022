module Test.Day03Spec (spec) where

import Day03
import Test.Hspec

spec :: SpecWith ()
spec =
  describe "Day 3 Solution" $ do
    it "Part 1" $ do
      part1Solution puzzleInput `shouldBe` 157

    it "Part 2" $ do
      part2Solution puzzleInput `shouldBe` 70