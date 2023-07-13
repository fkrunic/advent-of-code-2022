module Problems.Day06 (
  part1Solution,
  part2Solution,
) where

import Data.Set (fromList)

buildMarker :: Int -> String -> Int
buildMarker _ [] = undefined
buildMarker k input =
  if uniqueCount == k
    then k
    else 1 + buildMarker k (tail input)
 where
  (chunk, _) = splitAt k input
  chunkSet = fromList chunk
  uniqueCount = length chunkSet

part1Solution :: String -> Int
part1Solution = buildMarker 4

part2Solution :: String -> Int
part2Solution = buildMarker 14
