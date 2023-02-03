module Test.Day06Spec (spec) where

import Day06
import Test.Hspec

spec :: SpecWith ()
spec =
  describe "Day 6 Solution" $ do
    it "Part 1" $ do
      map part1Solution puzzleInputs `shouldBe` [5, 6, 10, 11]

    it "Part 2" $ do
      map part2Solution puzzleInputs `shouldBe` [23, 23, 29, 26]