module Test.Day08Spec (spec) where

import Data.List (intercalate)
import Problems.Day08 (part1Solution, part2Solution)
import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec =
  testGroup "Day 8 Solution" $
    [ testCase "Part 1" $
        part1Solution puzzleInput @?= 21

    , testCase "Part 2" $
        part2Solution puzzleInput @?= 8
    ]

puzzleInput :: String
puzzleInput =
  intercalate
    "\n"
    [ "30373"
    , "25512"
    , "65332"
    , "33549"
    , "35390"
    ]
