module Test.Day07Spec (spec) where

import Day07
import Test.Hspec

spec :: SpecWith ()
spec = 
  describe "Day 7 Solution" $ do
    it "Part 1" $ do
      part1Solution puzzleInput `shouldBe` 95437

    it "Part 2" $ do
      part2Solution puzzleInput `shouldBe` 24933642