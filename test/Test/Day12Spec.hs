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
    it "Converting Cells to Gridpoints" $ do
      toPoints parsedCells `shouldBe` parsedGPS

parser :: Text -> [[Cell]]
parser = fromRight [] . runParser (some (pLine <* optional newline)) ""

parsedGPS :: [GridPoint]
parsedGPS =
  [ ((StartCell, Height 0), (XCoordinate 0, YCoordinate 0)) -- S
  , ((GenericCell, Height 0), (XCoordinate 1, YCoordinate 0)) -- a
  , ((GenericCell, Height 1), (XCoordinate 2, YCoordinate 0)) -- b
  , ((GenericCell, Height 16), (XCoordinate 3, YCoordinate 0)) -- q
  , ((GenericCell, Height 15), (XCoordinate 4, YCoordinate 0)) -- p
  , ((GenericCell, Height 14), (XCoordinate 5, YCoordinate 0)) -- o
  , ((GenericCell, Height 13), (XCoordinate 6, YCoordinate 0)) -- n
  , ((GenericCell, Height 12), (XCoordinate 7, YCoordinate 0)) -- m
  , ((GenericCell, Height 0), (XCoordinate 0, YCoordinate 1)) -- a
  , ((GenericCell, Height 1), (XCoordinate 1, YCoordinate 1)) -- b
  , ((GenericCell, Height 2), (XCoordinate 2, YCoordinate 1)) -- c
  , ((GenericCell, Height 17), (XCoordinate 3, YCoordinate 1)) -- r
  , ((GenericCell, Height 24), (XCoordinate 4, YCoordinate 1)) -- y
  , ((GenericCell, Height 23), (XCoordinate 5, YCoordinate 1)) -- x
  , ((GenericCell, Height 23), (XCoordinate 6, YCoordinate 1)) -- x
  , ((GenericCell, Height 11), (XCoordinate 7, YCoordinate 1)) -- l
  , ((GenericCell, Height 0), (XCoordinate 0, YCoordinate 2)) -- a
  , ((GenericCell, Height 2), (XCoordinate 1, YCoordinate 2)) -- c
  , ((GenericCell, Height 2), (XCoordinate 2, YCoordinate 2)) -- c
  , ((GenericCell, Height 18), (XCoordinate 3, YCoordinate 2)) -- s
  , ((GenericCell, Height 25), (XCoordinate 4, YCoordinate 2)) -- z
  , ((EndCell, Height 25), (XCoordinate 5, YCoordinate 2)) -- E
  , ((GenericCell, Height 23), (XCoordinate 6, YCoordinate 2)) -- x
  , ((GenericCell, Height 10), (XCoordinate 7, YCoordinate 2)) -- k
  , ((GenericCell, Height 0), (XCoordinate 0, YCoordinate 3)) -- a
  , ((GenericCell, Height 2), (XCoordinate 1, YCoordinate 3)) -- c
  , ((GenericCell, Height 2), (XCoordinate 2, YCoordinate 3)) -- c
  , ((GenericCell, Height 19), (XCoordinate 3, YCoordinate 3)) -- t
  , ((GenericCell, Height 20), (XCoordinate 4, YCoordinate 3)) -- u
  , ((GenericCell, Height 21), (XCoordinate 5, YCoordinate 3)) -- v
  , ((GenericCell, Height 22), (XCoordinate 6, YCoordinate 3)) -- w
  , ((GenericCell, Height 9), (XCoordinate 7, YCoordinate 3)) -- v
  , ((GenericCell, Height 0), (XCoordinate 0, YCoordinate 4)) -- a
  , ((GenericCell, Height 1), (XCoordinate 1, YCoordinate 4)) -- b
  , ((GenericCell, Height 3), (XCoordinate 2, YCoordinate 4)) -- d
  , ((GenericCell, Height 4), (XCoordinate 3, YCoordinate 4)) -- e
  , ((GenericCell, Height 5), (XCoordinate 4, YCoordinate 4)) -- f
  , ((GenericCell, Height 6), (XCoordinate 5, YCoordinate 4)) -- g
  , ((GenericCell, Height 7), (XCoordinate 6, YCoordinate 4)) -- h
  , ((GenericCell, Height 8), (XCoordinate 7, YCoordinate 4)) -- i
  ]

parsedCells :: [[Cell]]
parsedCells =
  [
    [ (StartCell, Height 0) -- S
    , (GenericCell, Height 0) -- a
    , (GenericCell, Height 1) -- b
    , (GenericCell, Height 16) -- q
    , (GenericCell, Height 15) -- p
    , (GenericCell, Height 14) -- o
    , (GenericCell, Height 13) -- n
    , (GenericCell, Height 12) -- m
    ]
  ,
    [ (GenericCell, Height 0) -- a
    , (GenericCell, Height 1) -- b
    , (GenericCell, Height 2) -- c
    , (GenericCell, Height 17) -- r
    , (GenericCell, Height 24) -- y
    , (GenericCell, Height 23) -- x
    , (GenericCell, Height 23) -- x
    , (GenericCell, Height 11) -- l
    ]
  ,
    [ (GenericCell, Height 0) -- a
    , (GenericCell, Height 2) -- c
    , (GenericCell, Height 2) -- c
    , (GenericCell, Height 18) -- s
    , (GenericCell, Height 25) -- z
    , (EndCell, Height 25) -- E
    , (GenericCell, Height 23) -- x
    , (GenericCell, Height 10) -- k
    ]
  ,
    [ (GenericCell, Height 0) -- a
    , (GenericCell, Height 2) -- c
    , (GenericCell, Height 2) -- c
    , (GenericCell, Height 19) -- t
    , (GenericCell, Height 20) -- u
    , (GenericCell, Height 21) -- v
    , (GenericCell, Height 22) -- w
    , (GenericCell, Height 9) -- v
    ]
  ,
    [ (GenericCell, Height 0) -- a
    , (GenericCell, Height 1) -- b
    , (GenericCell, Height 3) -- d
    , (GenericCell, Height 4) -- e
    , (GenericCell, Height 5) -- f
    , (GenericCell, Height 6) -- g
    , (GenericCell, Height 7) -- h
    , (GenericCell, Height 8) -- i
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