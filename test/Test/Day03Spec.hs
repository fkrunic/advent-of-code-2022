module Test.Day03Spec (spec) where

import Data.Text (Text, intercalate)
import Day03
import Test.Hspec

spec :: SpecWith ()
spec =
  describe "Day 3 Solution" $ do
    it "Part 1" $ do
      part1Solution puzzleInput `shouldBe` 157

    it "Part 2" $ do
      part2Solution puzzleInput `shouldBe` 70

puzzleInput :: Text
puzzleInput =
  intercalate
    "\n"
    [ "vJrwpWtwJgWrhcsFMMfFFhFp"
    , "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
    , "PmmdzqPrVvPwwTWBwg"
    , "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
    , "ttgJtRGJQctTZtZT"
    , "CrZsJsPPZsGzwwsLwLmpwMDw"
    ]