{-# LANGUAGE TupleSections #-}

module Day12 where

import Data.Char (ord)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

newtype Height = Height Int deriving (Show, Eq, Ord)
newtype XCoordinate = XCoordinate Int deriving (Show, Eq, Ord)
newtype YCoordinate = YCoordinate Int deriving (Show, Eq, Ord)
type Coordinate = (XCoordinate, YCoordinate)

data Move = UpMove | DownMove | LeftMove | RightMove deriving (Show, Eq)

data CellType = StartCell | EndCell | GenericCell deriving (Show, Eq)
type Cell = (CellType, Height)
type GridPoint = (Cell, Coordinate)
type Path = [GridPoint]
type Grid = Map Coordinate Cell

--------------------------------------------------------------------------------

sc :: Parser ()
sc = L.space space1 empty empty

lexer :: Parser a -> Parser a
lexer = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

pCell :: Parser Cell
pCell =
  choice
    [ char 'S' $> (StartCell, Height 0)
    , char 'E' $> (EndCell, Height 25)
    , (GenericCell,) . Height . flip (-) 97 . ord <$> letterChar
    ]

pLine :: Parser [Cell]
pLine = some pCell

toPoints :: [[Cell]] -> Grid
toPoints xxs =
  M.fromList
    [ ((XCoordinate xCoord, YCoordinate yCoord), cell)
    | (yCoord, xs) <- zip [0 ..] xxs
    , (xCoord, cell) <- zip [0 ..] xs
    ]

canClimbTo :: Cell -> Cell -> Bool
canClimbTo (_, Height starting) (_, Height next) = next <= starting + 1