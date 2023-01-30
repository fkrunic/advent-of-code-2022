import Test.Hspec

import qualified Day1 as D01

main :: IO ()
main = hspec $ do
  describe "Advent of Code Tests" $ do 
    it "Day 1 Solution - Part 1" $ do
      D01.part1Solution D01.puzzleInput `shouldBe` 24000
    it "Day 1 Solution - Part 2" $ do
      D01.part2Solution D01.puzzleInput `shouldBe` 45000      
