module Main (main) where

import Data.Text qualified as T

import Day01 qualified as D01
import Day02 qualified as D02
import Day03 qualified as D03
import Day04 qualified as D04
import Day05 qualified as D05
import Day06 qualified as D06
import Day07 qualified as D07
import Day08 qualified as D08
import Day09 qualified as D09
import Day10 qualified as D10

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
  writeFile "outputs/day10p2.txt" $ T.unpack $ D10.part2Solution input

main :: IO ()
main = day10Main
