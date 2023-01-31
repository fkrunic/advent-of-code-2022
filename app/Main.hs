module Main (main) where

import qualified Data.Text as T

import qualified Day01 as D01
import qualified Day02 as D02
import qualified Day03 as D03
import qualified Day04 as D04
import qualified Day05 as D05
import qualified Day06 as D06

day01Main :: IO ()
day01Main = do 
  input <- readFile "inputs/day1.txt" 
  print (D01.part1Solution input)
  print (D01.part2Solution input)

day02Main :: IO () 
day02Main = do
  input <- T.pack <$> readFile "inputs/day2.txt"
  print (D02.part1Solution input)
  print (D02.part2Solution input)

day03Main :: IO () 
day03Main = do
  input <- T.pack <$> readFile "inputs/day3.txt"
  print (D03.part1Solution input)
  print (D03.part2Solution input)  

day04Main :: IO ()
day04Main = do 
  input <- T.pack <$> readFile "inputs/day4.txt"
  print (D04.part1Solution input)
  print (D04.part2Solution input)

day05Main :: IO ()
day05Main = do 
  input <- T.pack <$> readFile "inputs/day5.txt"
  print (D05.part1Solution input)
  print (D05.part2Solution input)

day06Main :: IO ()
day06Main = do 
  input <- readFile "inputs/day6.txt"
  print (D06.part1Solution input)
  print (D06.part2Solution input)  

main :: IO ()
main = day06Main
