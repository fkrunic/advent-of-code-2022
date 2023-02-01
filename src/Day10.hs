{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import Data.Bifunctor (first)
import           Data.Either                (fromRight)
import           Data.Functor               (($>))
import           Data.List                  (sortOn, uncons, scanr, find)
import           Data.Map                   (Map)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromJust)
import qualified Data.Set                   as S
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

lexer :: Parser a -> Parser a
lexer = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Int
integer = lexer L.decimal

signedInteger :: Parser Int
signedInteger = L.signed sc integer

data Instruction
  = AddX Int
  | Noop
  deriving (Show, Eq)

data CycleStart = CycleStart deriving (Show, Eq)
data CycleDuring = CycleDuring deriving (Show, Eq)
data CycleEnd = CycleEnd deriving (Show, Eq)
data Delayed = Delayed Instruction deriving (Show, Eq)

{-

Noop
cycle start -> cycle during -> cycle end

Addx

cycle start -> cycle during -> cycle end -> cycle start -> cycle during -> cycle end + update

-}

start :: CycleStart -> Instruction -> (CycleDuring, Maybe Delayed)
start = undefined

finish :: CycleDuring -> 

------------------------------------------------------------------------------------

pInstruction :: Parser Instruction
pInstruction =
  choice
    [ symbol "noop" $> Noop,
      AddX <$> (symbol "addx" *> signedInteger)
    ]

------------------------------------------------------------------------------------

run :: [Instruction] -> [(Int, Int)]
run = reverse . foldr builder [(1, 1)] . reverse 
  where
    builder Noop [] = undefined
    builder Noop acc@((cc, r):_) = (cc + 1, r):acc
    builder (AddX _) [] = undefined
    builder (AddX v) acc@((cc, r):_) = (cc + 2, r + v):(cc + 1, r):acc    

findNearestCycle :: Int -> [(Int, Int)] -> Maybe (Int, Int)
findNearestCycle cc = fmap (first (const cc)) . find (marker cc) . reverse
  where
    marker k (n, _) = n <= k

targetCycles :: [Int]
targetCycles = [20, 60, 100, 140, 180, 220]

------------------------------------------------------------------------------------

drawPixel :: Int -> Int -> Bool
drawPixel cc reg = reg - 1 <= k && k <= reg + 1
  where
    k = ((cc - 1) `mod` 40) + 1

------------------------------------------------------------------------------------

part1Solution :: Text -> Int
part1Solution = sum . map signal . filter (flip elem targetCycles . fst) . run . parse
  where
    parse = fromRight [] . runParser (some pInstruction) ""
    signal (a, b) = a * b

part2Solution :: Text -> [(Int, Int, Char)]
part2Solution = map draw . run . parse
  where
    parse = fromRight [] . runParser (some pInstruction) ""
    draw (cc, r) = if drawPixel cc r then (cc, r, '#') else (cc, r, '.')

smallInput :: Text
smallInput =
  T.intercalate
    "\n"
    [ "noop",
      "addx 3",
      "addx -5"
    ]

puzzleInput :: Text
puzzleInput =
  T.intercalate
    "\n"
    [ "addx 15",
      "addx -11",
      "addx 6",
      "addx -3",
      "addx 5",
      "addx -1",
      "addx -8",
      "addx 13",
      "addx 4",
      "noop",
      "addx -1",
      "addx 5",
      "addx -1",
      "addx 5",
      "addx -1",
      "addx 5",
      "addx -1",
      "addx 5",
      "addx -1",
      "addx -35",
      "addx 1",
      "addx 24",
      "addx -19",
      "addx 1",
      "addx 16",
      "addx -11",
      "noop",
      "noop",
      "addx 21",
      "addx -15",
      "noop",
      "noop",
      "addx -3",
      "addx 9",
      "addx 1",
      "addx -3",
      "addx 8",
      "addx 1",
      "addx 5",
      "noop",
      "noop",
      "noop",
      "noop",
      "noop",
      "addx -36",
      "noop",
      "addx 1",
      "addx 7",
      "noop",
      "noop",
      "noop",
      "addx 2",
      "addx 6",
      "noop",
      "noop",
      "noop",
      "noop",
      "noop",
      "addx 1",
      "noop",
      "noop",
      "addx 7",
      "addx 1",
      "noop",
      "addx -13",
      "addx 13",
      "addx 7",
      "noop",
      "addx 1",
      "addx -33",
      "noop",
      "noop",
      "noop",
      "addx 2",
      "noop",
      "noop",
      "noop",
      "addx 8",
      "noop",
      "addx -1",
      "addx 2",
      "addx 1",
      "noop",
      "addx 17",
      "addx -9",
      "addx 1",
      "addx 1",
      "addx -3",
      "addx 11",
      "noop",
      "noop",
      "addx 1",
      "noop",
      "addx 1",
      "noop",
      "noop",
      "addx -13",
      "addx -19",
      "addx 1",
      "addx 3",
      "addx 26",
      "addx -30",
      "addx 12",
      "addx -1",
      "addx 3",
      "addx 1",
      "noop",
      "noop",
      "noop",
      "addx -9",
      "addx 18",
      "addx 1",
      "addx 2",
      "noop",
      "noop",
      "addx 9",
      "noop",
      "noop",
      "noop",
      "addx -1",
      "addx 2",
      "addx -37",
      "addx 1",
      "addx 3",
      "noop",
      "addx 15",
      "addx -21",
      "addx 22",
      "addx -6",
      "addx 1",
      "noop",
      "addx 2",
      "addx 1",
      "noop",
      "addx -10",
      "noop",
      "noop",
      "addx 20",
      "addx 1",
      "addx 2",
      "addx 2",
      "addx -6",
      "addx -11",
      "noop",
      "noop",
      "noop"
    ]
