module Test.Day01Spec (spec) where

import Day01
import Test.Hspec

spec :: SpecWith ()
spec =
  it "Day 1 Solution - Part 1" $ do
    part1Solution puzzleInput `shouldBe` 24000