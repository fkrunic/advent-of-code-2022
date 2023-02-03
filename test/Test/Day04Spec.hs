module Test.Day04Spec (spec) where

import Day04
import Test.Hspec

spec :: SpecWith ()
spec =
  describe "Day 4 Solution" $ do
    it "Part 1" $ do
      part1Solution puzzleInput `shouldBe` 2

    it "Part 2" $ do
      part2Solution puzzleInput `shouldBe` 4