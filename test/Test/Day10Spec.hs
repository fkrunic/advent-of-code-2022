module Test.Day10Spec (spec) where

import Day10
import Test.Hspec
import Data.Text (Text)
import Data.Text qualified as T

spec :: SpecWith ()
spec =
  describe "Day 10" $ do
    it "Part 1" $ do
      part1Solution puzzleInput `shouldBe` 13140

    it "Part 2" $ do
      part2Solution puzzleInput `shouldBe` crtTestImage

crtTestImage :: Text
crtTestImage = 
  T.intercalate "\n"
   [ "##..##..##..##..##..##..##..##..##..##.."
   , "###...###...###...###...###...###...###."
   , "####....####....####....####....####...."
   , "#####.....#####.....#####.....#####....."
   , "######......######......######......####"
   , "#######.......#######.......#######....."
   ]