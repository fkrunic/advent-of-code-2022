module Main (main) where

import qualified Data.Text as T

import qualified Day1 as D01
import qualified Day2 as D02
import qualified Day3 as D03

day1Main :: IO ()
day1Main = do 
  input <- readFile "inputs/day1.txt" 
  print (D01.part1Solution input)
  print (D01.part2Solution input)

day2Main :: IO () 
day2Main = do
  input <- T.pack <$> readFile "inputs/day2.txt"
  print (D02.part1Solution input)
  print (D02.part2Solution input)

day3Main :: IO () 
day3Main = do
  input <- T.pack <$> readFile "inputs/day3.txt"
  print (D03.part1Solution input)
  print (D03.part2Solution input)  

main :: IO ()
main = day3Main
