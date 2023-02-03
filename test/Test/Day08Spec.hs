module Test.Day08Spec (spec) where

import Day08
import Test.Hspec

spec :: SpecWith ()
spec =
  describe "Day 8 Solution" $ do
    it "Part 1" $ do
      part1Solution puzzleInput `shouldBe` 21

    it "Part 2" $ do
      part2Solution puzzleInput `shouldBe` 8