module Test.Day09Spec (spec) where

import Data.Text (Text, intercalate)
import Problems.Day09 (part1Solution, part2Solution)
import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec =
  testGroup "Day 9 Solution" $
    [ testCase "Part 1" $
        part1Solution puzzleInput @?= 13

    , testCase "Part 2" $
        part2Solution largerPuzzleInput @?= 36
    ]
    
puzzleInput :: Text
puzzleInput =
  intercalate
    "\n"
    [ "R 4"
    , "U 4"
    , "L 3"
    , "D 1"
    , "R 4"
    , "D 1"
    , "L 5"
    , "R 2"
    ]

largerPuzzleInput :: Text
largerPuzzleInput =
  intercalate
    "\n"
    [ "R 5"
    , "U 8"
    , "L 8"
    , "D 3"
    , "R 17"
    , "D 10"
    , "L 25"
    , "U 20"
    ]
