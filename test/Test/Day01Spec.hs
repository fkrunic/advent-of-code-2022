module Test.Day01Spec (spec) where

import Day01
import Test.Hspec

spec :: SpecWith ()
spec =
  describe "Day 1 Solution" $ do
    it "Part 1" $ do
      part1Solution puzzleInput `shouldBe` 24000

    it "Part 2" $ do
      part2Solution puzzleInput `shouldBe` 45000