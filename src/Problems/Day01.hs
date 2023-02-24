module Problems.Day01 (
  part1Solution,
  part2Solution,
) where

import Data.List (sortOn)

calorieChunker :: [String] -> [Int]
calorieChunker [] = []
calorieChunker cls =
  if null chunk
    then calorieChunker (tail rest)
    else total : calorieChunker rest
 where
  (chunk, rest) = span (/= "") cls
  total = (sum . map read) chunk

part1Solution :: String -> Int
part1Solution =
  snd
    . head
    . sortOn (negate . snd)
    . zip [1 :: Int ..]
    . calorieChunker
    . lines

part2Solution :: String -> Int
part2Solution =
  sum
    . map snd
    . take 3
    . sortOn (negate . snd)
    . zip [1 :: Int ..]
    . calorieChunker
    . lines
