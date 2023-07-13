module Test.Day02Spec (spec) where

import Data.Text (Text)
import Problems.Day02 (part1Solution, part2Solution)
import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec =
  testGroup "Day 2 Solution" $
    [ testCase "Part 1" $
        part1Solution puzzleInput @?= 15

    , testCase "Part 2" $
        part2Solution puzzleInput @?= 12
    ]
    
puzzleInput :: Text
puzzleInput = "A Y\nB X\nC Z"
