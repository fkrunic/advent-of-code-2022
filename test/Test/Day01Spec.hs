module Test.Day01Spec (spec) where

import Data.List (intercalate)
import Day01
import Test.Hspec

spec :: SpecWith ()
spec =
  describe "Day 1 Solution" $ do
    it "Part 1" $ do
      part1Solution puzzleInput `shouldBe` 24000

    it "Part 2" $ do
      part2Solution puzzleInput `shouldBe` 45000

puzzleInput :: String
puzzleInput =
  intercalate
    "\n"
    [ "1000"
    , "2000"
    , "3000"
    , ""
    , "4000"
    , ""
    , "5000"
    , "6000"
    , ""
    , "7000"
    , "8000"
    , "9000"
    , ""
    , "10000"
    ]