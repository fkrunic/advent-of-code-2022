module Test.Day05Spec (spec) where

import Data.Text (Text, intercalate)
import Problems.Day05 (part1Solution, part2Solution)
import Test.Hspec (SpecWith, describe, it, shouldBe)

spec :: SpecWith ()
spec =
  describe "Day 5 Solution" $ do
    it "Part 1" $ do
      part1Solution puzzleInput `shouldBe` "CMZ"

    it "Part 2" $ do
      part2Solution puzzleInput `shouldBe` "MCD"

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
