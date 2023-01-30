import Test.Hspec

import qualified Day01 as D01
import qualified Day02 as D02
import qualified Day03 as D03
import qualified Day04 as D04

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

