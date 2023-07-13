module Test.Day06Spec (spec) where

import Problems.Day06 (part1Solution, part2Solution)
import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec =
  testGroup "Day 6 Solution" $
    [ testCase "Part 1" $
        map part1Solution puzzleInput @?= [5, 6, 10, 11]

    , testCase "Part 2" $
        map part2Solution puzzleInput @?= [23, 23, 29, 26]
    ]

puzzleInput :: [String]
puzzleInput =
  [ "bvwbjplbgvbhsrlpgdmjqwftvncz"
  , "nppdvjthqldpwncqszvftbrmjlhg"
  , "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
  , "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
  ]
