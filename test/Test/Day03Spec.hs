module Test.Day03Spec (spec) where

import Data.Text (Text, intercalate)
import Problems.Day03 (part1Solution, part2Solution)
import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec =
  testGroup "Day 3 Solution" $
    [ testCase "Part 1" $
        part1Solution puzzleInput @?= 157

    , testCase "Part 2" $
        part2Solution puzzleInput @?= 70
    ]

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
