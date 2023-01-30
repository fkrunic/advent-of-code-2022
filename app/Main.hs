module Main (main) where

import qualified Day1 as D01

day1Main :: IO ()
day1Main = do 
  input <- readFile "inputs/day1.txt" 
  print (D01.part1Solution input)
  print (D01.part2Solution input)

main :: IO ()
main = day1Main
