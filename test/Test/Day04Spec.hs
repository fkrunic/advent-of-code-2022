module Test.Day04Spec (spec) where

import Data.Text (Text, intercalate)
import Problems.Day04 (part1Solution, part2Solution)
import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec =
  testGroup "Day 4 Solution" $
    [ testCase "Part 1" $
        part1Solution puzzleInput @?= 2

    , testCase "Part 2" $
        part2Solution puzzleInput @?= 4
    ]
    
puzzleInput :: Text
puzzleInput =
  intercalate
    "\n"
    [ "2-4,6-8"
    , "2-3,4-5"
    , "5-7,7-9"
    , "2-8,3-7"
    , "6-6,4-6"
    , "2-6,4-8"
    ]
