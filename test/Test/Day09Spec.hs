module Test.Day09Spec (spec) where

import Data.Text (Text, intercalate)
import Problems.Day09 (
  part1Solution,
  part2Solution,
 )
import Test.Hspec (SpecWith, describe, it, shouldBe)

spec :: SpecWith ()
spec =
  describe "Day 9 Solution" $ do
    it "Part 1" $ do
      part1Solution puzzleInput `shouldBe` 13

    it "Part 2" $ do
      part2Solution largerPuzzleInput `shouldBe` 36

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
