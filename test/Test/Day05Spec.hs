module Test.Day05Spec (spec) where

import Day05
import Test.Hspec

spec :: SpecWith ()
spec =
  describe "Day 5 Solution" $ do
    it "Part 1" $ do
      part1Solution puzzleInput `shouldBe` "CMZ"

    it "Part 2" $ do
      part2Solution puzzleInput `shouldBe` "MCD"