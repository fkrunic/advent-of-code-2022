module Test.Day05Spec (spec) where

import Data.Text (Text, intercalate)
import Problems.Day05 (part1Solution, part2Solution)
import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec =
  testGroup "Day 5 Solution" $
    [ testCase "Part 1" $
        part1Solution puzzleInput @?= "CMZ"

    , testCase "Part 2" $
        part2Solution puzzleInput @?= "MCD"
    ]
    
    
puzzleInput :: Text
puzzleInput =
  intercalate
    "\n"
    [ "    [D]    "
    , "[N] [C]    "
    , "[Z] [M] [P]"
    , " 1   2   3 "
    , ""
    , "move 1 from 2 to 1"
    , "move 3 from 1 to 3"
    , "move 2 from 2 to 1"
    , "move 1 from 1 to 2"
    ]
