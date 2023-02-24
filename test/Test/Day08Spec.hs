module Test.Day08Spec (spec) where

import Data.List (intercalate)
import Problems.Day08 (part1Solution, part2Solution)
import Test.Hspec (SpecWith, describe, it, shouldBe)

spec :: SpecWith ()
spec =
  describe "Day 8 Solution" $ do
    it "Part 1" $ do
      part1Solution puzzleInput `shouldBe` 21

    it "Part 2" $ do
      part2Solution puzzleInput `shouldBe` 8

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
