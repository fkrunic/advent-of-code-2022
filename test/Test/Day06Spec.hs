module Test.Day06Spec (spec) where

import Problems.Day06 (part1Solution, part2Solution)
import Test.Hspec (SpecWith, describe, it, shouldBe)

spec :: SpecWith ()
spec =
  describe "Day 6 Solution" $ do
    it "Part 1" $ do
      map part1Solution puzzleInput `shouldBe` [5, 6, 10, 11]

    it "Part 2" $ do
      map part2Solution puzzleInput `shouldBe` [23, 23, 29, 26]

puzzleInput :: [String]
puzzleInput =
  [ "bvwbjplbgvbhsrlpgdmjqwftvncz"
  , "nppdvjthqldpwncqszvftbrmjlhg"
  , "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
  , "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
  ]
