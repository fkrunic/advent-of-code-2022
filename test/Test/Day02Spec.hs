module Test.Day02Spec (spec) where

import Data.Text (Text)
import Problems.Day02 (part1Solution, part2Solution)
import Test.Hspec (SpecWith, describe, it, shouldBe)

spec :: SpecWith ()
spec =
  describe "Day 2 Solution" $ do
    it "Part 1" $ do
      part1Solution puzzleInput `shouldBe` 15

    it "Part 2" $ do
      part2Solution puzzleInput `shouldBe` 12

puzzleInput :: Text
puzzleInput = "A Y\nB X\nC Z"
