module Day1 where

import Data.List (sortOn, intercalate)

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
part1Solution = snd . head . sortOn (negate . snd) . zip [1..] . calorieChunker . lines

part2Solution :: String -> Int 
part2Solution = sum . map snd . take 3 . sortOn (negate . snd) . zip [1..] . calorieChunker . lines

puzzleInput :: String
puzzleInput = intercalate "\n"
  [ "1000"
  , "2000"
  , "3000"
  , ""
  , "4000"
  , ""
  , "5000"
  , "6000"
  , ""
  , "7000"
  , "8000"
  , "9000"
  , ""
  , "10000"
  ]