module Utilities.Patterns where

import Data.IntSet qualified as IS
import Data.List (group, groupBy)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Math.NumberTheory.ArithmeticFunctions

newtype Divisor = Divisor Int deriving (Show, Eq)
newtype Offset = Offset Int deriving (Show, Eq)
newtype PatternLength = PatternLength Int deriving (Show, Eq)

findPattern :: Eq a => Int -> [a] -> (Offset, Maybe PatternLength)
findPattern k xs = foldr checker Nothing [0 .. length xs]
 where
  xsLength = length xs
  checker i acc =
    let (front, back) = splitAt i xs
        chopped = chop i back
     in if length (group chopped) == 1
          then (Offset, Just $ PatternLength xsLength)
          else (Offset, Nothing)

hasPattern :: Eq a => Offset -> PatternLength -> [a] -> Bool
hasPattern (Offset offset) (PatternLength pattern) xs =
  length (group chopped) == 1
 where
  (front, back) = splitAt offset xs
  chopped = chop pattern xs

getDivisors :: Int -> [Divisor]
getDivisors =
  NE.toList
    . NE.map Divisor
    . NE.fromList
    . IS.toList
    . divisorsSmall

chop :: Int -> [a] -> [[a]]
chop k =
  map (map snd)
    . groupBy (\t1 t2 -> fst t1 == fst t2)
    . zipWith (\i x -> (i `div` k, x)) [0 :: Int ..]
