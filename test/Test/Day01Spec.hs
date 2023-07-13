module Test.Day01Spec (spec) where

import Data.List (intercalate)
import Problems.Day01 (part1Solution, part2Solution)
import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec =
  testGroup "Day 1 Solution" $
    [ testCase "Part 1" $
        part1Solution puzzleInput @?= 24000

    , testCase "Part 2" $
        part2Solution puzzleInput @?= 45000
    ]

puzzleInput :: String
puzzleInput =
  intercalate
    "\n"
    [ "1000"
    , "2000"
    , "3000"
    , ""
    , "4000"
    , ""
    , "5000"
    , "6000"
    , ""
    , "7000"
    , "8000"
    , "9000"
    , ""
    , "10000"
    ]
