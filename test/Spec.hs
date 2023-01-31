{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import qualified Day01 as D01
import qualified Day02 as D02
import qualified Day03 as D03
import qualified Day04 as D04
import qualified Day05 as D05
import qualified Day06 as D06
import qualified Day07 as D07

main :: IO ()
main = hspec $ do
  describe "Advent of Code Tests" $ do 
    it "Day 1 Solution - Part 1" $ do
      D01.part1Solution D01.puzzleInput `shouldBe` 24000
    it "Day 1 Solution - Part 2" $ do
      D01.part2Solution D01.puzzleInput `shouldBe` 45000    
    it "Day 2 Solution - Part 1" $ do 
      D02.part1Solution D02.puzzleInput `shouldBe` 15
    it "Day 2 Solution - Part 2" $ do 
      D02.part2Solution D02.puzzleInput `shouldBe` 12
    it "Day 3 Solution - Part 1" $ do 
      D03.part1Solution D03.puzzleInput `shouldBe` 157
    it "Day 3 Solution - Part 2" $ do 
      D03.part2Solution D03.puzzleInput `shouldBe` 70      
    it "Day 4 Solution - Part 1" $ do
      D04.part1Solution D04.puzzleInput `shouldBe` 2
    it "Day 4 Solution - Part 2" $ do
      D04.part2Solution D04.puzzleInput `shouldBe` 4
    it "Day 5 Solution - Part 1" $ do
      D05.part1Solution D05.puzzleInput `shouldBe` "CMZ"
    it "Day 5 Solution - Part 2" $ do
      D05.part2Solution D05.puzzleInput `shouldBe` "MCD"
    it "Day 6 Solution - Part 1" $ do
      map D06.part1Solution D06.puzzleInputs `shouldBe` [5, 6, 10, 11]
    it "Day 6 Solution - Part 2" $ do
      map D06.part2Solution D06.puzzleInputs `shouldBe` [23, 23, 29, 26]
    it "Day 7 Solution - Part 1" $ do
      D07.part1Solution D07.puzzleInput `shouldBe` 95437
    it "Day 7 Solution - Part 2" $ do
      D07.part2Solution D07.puzzleInput `shouldBe` 24933642


