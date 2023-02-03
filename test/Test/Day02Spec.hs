module Test.Day02Spec (spec) where

import Day02
import Test.Hspec

spec :: SpecWith ()
spec =
  describe "Day 2 Solution" $ do
    it "Part 1" $ do
      part1Solution puzzleInput `shouldBe` 15

    it "Part 2" $ do
      part2Solution puzzleInput `shouldBe` 12