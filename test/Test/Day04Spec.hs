module Test.Day04Spec (spec) where

import Data.Text (Text, intercalate)
import Problems.Day04 (part1Solution, part2Solution)
import Test.Hspec (SpecWith, describe, it, shouldBe)

spec :: SpecWith ()
spec =
  describe "Day 4 Solution" $ do
    it "Part 1" $ do
      part1Solution puzzleInput `shouldBe` 2

    it "Part 2" $ do
      part2Solution puzzleInput `shouldBe` 4

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
