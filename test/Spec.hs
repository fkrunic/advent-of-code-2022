import Test.Hspec

import qualified Day1 as D01
import qualified Day2 as D02

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
