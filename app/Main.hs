module Main (main) where

import qualified Data.Text as T

import qualified Day01 as D01
import qualified Day02 as D02
import qualified Day03 as D03
import qualified Day04 as D04
import qualified Day05 as D05
import qualified Day06 as D06
import qualified Day07 as D07
import qualified Day08 as D08
import qualified Day09 as D09
import qualified D10Round3 as D10

day01Main :: IO ()
day01Main = do 
  input <- readFile "inputs/day01.txt" 
  print (D01.part1Solution input)
  print (D01.part2Solution input)

day02Main :: IO () 
day02Main = do
  input <- T.pack <$> readFile "inputs/day02.txt"
  print (D02.part1Solution input)
  print (D02.part2Solution input)

day03Main :: IO () 
day03Main = do
  input <- T.pack <$> readFile "inputs/day03.txt"
  print (D03.part1Solution input)
  print (D03.part2Solution input)  

day04Main :: IO ()
day04Main = do 
  input <- T.pack <$> readFile "inputs/day04.txt"
  print (D04.part1Solution input)
  print (D04.part2Solution input)

day05Main :: IO ()
day05Main = do 
  input <- T.pack <$> readFile "inputs/day05.txt"
  print (D05.part1Solution input)
  print (D05.part2Solution input)

day06Main :: IO ()
day06Main = do 
  input <- readFile "inputs/day06.txt"
  print (D06.part1Solution input)
  print (D06.part2Solution input)  

day07Main :: IO ()
day07Main = do 
  input <- T.pack <$> readFile "inputs/day07.txt"
  print (D07.part1Solution input)
  print (D07.part2Solution input)

day08Main :: IO ()
day08Main = do 
  input <- readFile "inputs/day08.txt"
  print (D08.part1Solution input)
  print (D08.part2Solution input)  

day09Main :: IO ()
day09Main = do
  input <- T.pack <$> readFile "inputs/day09.txt"
  print (D09.part1Solution input)  
  print (D09.part2Solution input)
  
day10Main :: IO ()
day10Main = do 
  input <- T.pack <$> readFile "inputs/day10.txt"
  print (D10.part1Solution input)
  D10.part2Solution input

main :: IO ()
main = day10Main
