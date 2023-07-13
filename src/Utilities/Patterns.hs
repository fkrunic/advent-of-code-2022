module Utilities.Patterns where

import Data.IntSet qualified as IS
import Data.List (group, groupBy, sort)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Math.NumberTheory.ArithmeticFunctions (divisorsSmall)

newtype Divisor = Divisor Int deriving (Show, Eq, Ord)
newtype Offset = Offset Int deriving (Show, Eq)
newtype PatternLength = PatternLength Int deriving (Show, Eq)
type Diff a = ([a], [a])

--------------------------------------------------------------------------------

getPatterns :: Eq a => [a] -> [(Offset, PatternLength)]
getPatterns xs =
  [ (Offset offset, PatternLength pl)
  | offset <- [0 .. length xs - 1]
  , Divisor pl <- getDivisors (length xs - offset)
  , hasPattern (Offset offset) (PatternLength pl) xs
  , 0 < pl && pl < length xs - offset
  ]

hasPattern :: Eq a => Offset -> PatternLength -> [a] -> Bool
hasPattern (Offset offset) (PatternLength pattern) xs =
  length (group chopped) == 1
 where
  (_, back) = splitAt offset xs
  chopped = chop pattern back

getDivisors :: Int -> [Divisor]
getDivisors = sort . map Divisor . IS.toList . divisorsSmall

chop :: Int -> [a] -> [[a]]
chop k =
  map (map snd)
    . groupBy (\t1 t2 -> fst t1 == fst t2)
    . zipWith (\i x -> (i `div` k, x)) [0 :: Int ..]

--------------------------------------------------------------------------------

diff :: Eq a => [a] -> [a] -> Diff a
diff [] xs = ([], xs)
diff xs [] = (xs, [])
diff xs@(x : xRest) ys@(y : yRest) =
  if x == y
    then diff xRest yRest
    else (xs, ys)

data Diffable a = Diffable
  { first :: [a]
  , second :: [a]
  , rest :: [[a]]
  }
  deriving (Show, Eq, Ord)

diffSequence :: Eq a => Diffable a -> NonEmpty (Diff a)
diffSequence (Diffable first second rest) = diff first second :| diffRest
 where
  diffRest = case rest of
    [] -> []
    (x : xs) -> NE.toList $ diffSequence (Diffable second x xs)

makeDfb :: [[a]] -> Maybe (Diffable a)
makeDfb (x : y : ys) = Just (Diffable x y ys)
makeDfb _ = Nothing
