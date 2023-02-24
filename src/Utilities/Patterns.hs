module Utilities.Patterns where

import Data.IntSet qualified as IS
import Data.List (groupBy)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Math.NumberTheory.ArithmeticFunctions

newtype Divisor = Divisor Int deriving (Show, Eq)
newtype Offset = Offset Int deriving (Show, Eq)
newtype PatternLength = PatternLength Int deriving (Show, Eq)

findPattern :: Eq a => [a] -> (Offset, Maybe PatternLength)
findPattern = undefined

getDivisors :: Int -> NonEmpty Divisor
getDivisors = NE.map Divisor . NE.fromList . IS.toList . divisorsSmall

equalSizeGroups :: Int -> [a] -> [[a]]
equalSizeGroups k =
  map (map snd)
    . groupBy (\t1 t2 -> fst t1 == fst t2)
    . zipWith (\i x -> (i `div` k, x)) [1 :: Int ..]

