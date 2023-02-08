module Test.Day12Spec (spec) where

import Test.Hspec

import Data.Either (fromRight)
import Data.Text (Text)
import Data.Text qualified as T
import Day12
import Text.Megaparsec
import Text.Megaparsec.Char

spec :: SpecWith ()
spec =
  describe "Day 12 Tests" $ do
    it "Parsing Puzzle Input" $ do
      parser puzzleInput `shouldBe` parsedCells

parser :: Text -> [[Cell]]
parser = fromRight [] . runParser (some (pLine <* optional newline)) ""

parsedCells :: [[Cell]]
parsedCells = 
  [ [ (StartCell, 0) -- S
    , (GenericCell, 0) -- a
    , (GenericCell, 1) -- b
    , (GenericCell, 16) -- q
    , (GenericCell, 15) -- p
    , (GenericCell, 14) -- o
    , (GenericCell, 13) -- n
    , (GenericCell, 12) -- m
    ]
  ,
    [ (GenericCell, 0) -- a
    , (GenericCell, 1) -- b
    , (GenericCell, 2) -- c
    , (GenericCell, 17) -- r
    , (GenericCell, 24) -- y
    , (GenericCell, 23) -- x
    , (GenericCell, 23) -- x
    , (GenericCell, 11) -- l
    ]
  ,
    [ (GenericCell, 0) -- a
    , (GenericCell, 2) -- c
    , (GenericCell, 2) -- c
    , (GenericCell, 18) -- s
    , (GenericCell, 25) -- z
    , (EndCell, 25) -- E
    , (GenericCell, 23) -- x
    , (GenericCell, 10) -- k
    ]
  ,
    [ (GenericCell, 0) -- a
    , (GenericCell, 2) -- c
    , (GenericCell, 2) -- c
    , (GenericCell, 19) -- t
    , (GenericCell, 20) -- u
    , (GenericCell, 21) -- v
    , (GenericCell, 22) -- w
    , (GenericCell, 9) -- v
    ]
  ,
    [ (GenericCell, 0) -- a
    , (GenericCell, 1) -- b
    , (GenericCell, 3) -- d
    , (GenericCell, 4) -- e
    , (GenericCell, 5) -- f
    , (GenericCell, 6) -- g
    , (GenericCell, 7) -- h
    , (GenericCell, 8) -- i
    ]
  ]

puzzleInput :: Text
puzzleInput =
  T.intercalate
    "\n"
    [ "Sabqponm"
    , "abcryxxl"
    , "accszExk"
    , "acctuvwj"
    , "abdefghi"
    ]