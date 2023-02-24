module Utilities.Patterns where

import Data.IntSet qualified as IS
import Data.List (group, groupBy, sort)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Math.NumberTheory.ArithmeticFunctions

newtype Divisor = Divisor Int deriving (Show, Eq, Ord)
newtype Offset = Offset Int deriving (Show, Eq)
newtype PatternLength = PatternLength Int deriving (Show, Eq)

getPatterns :: Eq a => [a] -> [(Offset, PatternLength)]
getPatterns xs =
  [ (Offset offset, PatternLength pl)
  | offset <- [0 .. length xs - 1]
  , Divisor pl <- getDivisors (length xs - offset)
  , hasPattern (Offset offset) (PatternLength pl) xs
  ]

hasPattern :: Eq a => Offset -> PatternLength -> [a] -> Bool
hasPattern (Offset offset) (PatternLength pattern) xs =
  length (group chopped) == 1
 where
  (front, back) = splitAt offset xs
  chopped = chop pattern xs

getDivisors :: Int -> [Divisor]
getDivisors =
  sort
    . NE.toList
    . NE.map Divisor
    . NE.fromList
    . IS.toList
    . divisorsSmall

chop :: Int -> [a] -> [[a]]
chop k =
  map (map snd)
    . groupBy (\t1 t2 -> fst t1 == fst t2)
    . zipWith (\i x -> (i `div` k, x)) [0 :: Int ..]
