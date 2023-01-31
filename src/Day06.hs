module Day06 where

import qualified Data.Set as S

buildMarker :: Int -> String -> Int
buildMarker _ [] = -1
buildMarker k input = 
  if uniqueCount == k
    then k
    else 1 + buildMarker k (tail input)
  where
    (chunk, rest) = splitAt k input
    chunkSet = S.fromList chunk
    uniqueCount = length chunkSet

part1Solution :: String -> Int
part1Solution = buildMarker 4

part2Solution :: String -> Int
part2Solution = buildMarker 14

puzzleInputs :: [String]
puzzleInputs = 
  [ "bvwbjplbgvbhsrlpgdmjqwftvncz"
  , "nppdvjthqldpwncqszvftbrmjlhg"
  , "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
  , "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
  ]