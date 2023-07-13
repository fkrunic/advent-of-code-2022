module Problems.Day12 where

import Data.Char (ord)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, empty, some)
import Text.Megaparsec.Char (char, letterChar, space1)
import Text.Megaparsec.Char.Lexer qualified as L

import Utilities.Graphs (Distance (Finite), Vertex (..))

type Parser = Parsec Void Text

newtype Height = Height Int deriving (Show, Eq, Ord)
newtype XCoordinate = XCoordinate Int deriving (Show, Eq, Ord)
newtype YCoordinate = YCoordinate Int deriving (Show, Eq, Ord)
type Coordinate = (XCoordinate, YCoordinate)

data Move = UpMove | DownMove | LeftMove | RightMove deriving (Show, Eq, Enum)

data CellType = StartCell | EndCell | GenericCell deriving (Show, Eq, Ord)
type Cell = (CellType, Height)
type GridPoint = (Coordinate, Cell)
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

canClimbFrom :: Cell -> Cell -> Bool
canClimbFrom (_, Height starting) (_, Height next) = next <= starting + 1

shift :: Move -> Coordinate -> Coordinate
shift UpMove (xCoord, YCoordinate y) = (xCoord, YCoordinate (y - 1))
shift DownMove (xCoord, YCoordinate y) = (xCoord, YCoordinate (y + 1))
shift LeftMove (XCoordinate x, yCoord) = (XCoordinate (x - 1), yCoord)
shift RightMove (XCoordinate x, yCoord) = (XCoordinate (x + 1), yCoord)

nextMoves :: GridPoint -> Grid -> [GridPoint]
nextMoves (coordinate, cell) grid = possibleGPS
 where
  moveOptions = map (`shift` coordinate) [UpMove .. RightMove]

  scanCells :: [Coordinate] -> [GridPoint]
  scanCells =
    filter (canClimbFrom cell . snd)
      . fromMaybe []
      . sequence
      . filter isJust
      . map (\coord -> M.lookup coord grid >>= Just . (coord,))

  possibleGPS = scanCells moveOptions

--------------------------------------------------------------------------------

getEdges :: Grid -> Vertex GridPoint -> Map (Vertex GridPoint) Distance
getEdges grid (Vertex gp) = 
  M.fromList $ map (\edge -> (Vertex edge, Finite 1)) $ nextMoves gp grid

vertices :: Grid -> [Vertex GridPoint]
vertices = map Vertex . M.assocs
