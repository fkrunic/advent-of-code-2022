module Test.Day09Spec (spec) where

import Day09
import Test.Hspec

spec :: SpecWith ()
spec =
  describe "Day 9 Solution" $ do
    it "Part 1" $ do
      part1Solution puzzleInput `shouldBe` 13

    it "Part 2" $ do
      part2Solution puzzleInput `shouldBe` 36