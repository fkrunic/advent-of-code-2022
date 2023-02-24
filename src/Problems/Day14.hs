module Problems.Day14 (
  DrawPath,
  Element,
  chainPath,
  defineGrid,
  defineGridNoAbyss,
  fillNStep,
  fillStep,
  fillStepSourceBlock,
  pDrawPath,
  pointsAlong,
  renderGrid,
) where

import Data.Foldable (foldrM)
import Data.Functor (($>))
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Text (Text)

import Utilities.Parsing (Parser, symbol)

import Utilities.Grids (
  Boundaries (Boundaries),
  Coordinate,
  Grid,
  XCoordinate (XCoordinate),
  YCoordinate (YCoordinate),
  drawGrid,
  getBounds,
  padBounds,
  point,
  unpackX,
  unpackY,
 )

import Text.Megaparsec (empty, optional, some)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L

type DrawPath = [Coordinate]
data Element = Sand | Rock | Air | Abyss | Source deriving (Show, Eq)

data DropOutcome
  = FallIntoAbyss
  | LandOnBlocker Coordinate
  | BlockSource Coordinate
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------

scKeepsNewLine :: Parser ()
scKeepsNewLine = L.space (some (char ' ') $> ()) empty empty

newlineSafeLexer :: Parser a -> Parser a
newlineSafeLexer = L.lexeme scKeepsNewLine

spaceInt :: Parser Int
spaceInt = newlineSafeLexer L.decimal

pVector :: Parser Coordinate
pVector =
  (,)
    <$> (XCoordinate <$> spaceInt <* symbol ",")
    <*> (YCoordinate <$> spaceInt)

pDrawPath :: Parser DrawPath
pDrawPath = some (pVector <* optional (symbol "->"))

--------------------------------------------------------------------------------

numbersBetween :: Int -> Int -> [Int]
numbersBetween x y
  | x <= y = [x .. y]
  | otherwise = reverse [y .. x]

pointsAlong :: Coordinate -> Coordinate -> Maybe [Coordinate]
pointsAlong c1 c2
  | fst c1 == fst c2 = Just yLine
  | snd c1 == snd c2 = Just xLine
  | otherwise = Nothing
 where
  xLine =
    map (\x -> (XCoordinate x, snd c1)) $
      numbersBetween (unpackX $ fst c1) (unpackX $ fst c2)

  yLine =
    map (\y -> (fst c1, YCoordinate y)) $
      numbersBetween (unpackY $ snd c1) (unpackY $ snd c2)

chainPath :: [Coordinate] -> Maybe [Coordinate]
chainPath = foldr builder (Just [])
 where
  builder _ Nothing = Nothing
  builder coord (Just []) = Just [coord]
  builder coord (Just (x : rest)) = (++ rest) <$> pointsAlong coord x

bulkAdjust :: Ord k => [k] -> (v -> v) -> Map k v -> Map k v
bulkAdjust keys update m = foldr (M.adjust update) m keys

defineGrid :: [DrawPath] -> Maybe (Grid Element)
defineGrid dps = do
  rocks <- concat <$> mapM chainPath dps
  let combined = source : rocks
      Boundaries gridXMin gridXMax gridYMin gridYMax =
        padBounds 1 $ getBounds combined
      abyssRow = [(xCoord, gridYMax) | xCoord <- [gridXMin .. gridXMax]]
      gridCoords =
        [ (xCoord, yCoord)
        | xCoord <- [gridXMin .. gridXMax]
        , yCoord <- [gridYMin .. gridYMax]
        ]
      canvas = M.fromList $ map (,Air) gridCoords
      grid =
        M.adjust (const Source) source $
          bulkAdjust abyssRow (const Abyss) $
            bulkAdjust rocks (const Rock) canvas
  return grid

defineGridNoAbyss :: [DrawPath] -> Maybe (Grid Element)
defineGridNoAbyss dps = do
  rocks <- concat <$> mapM chainPath dps
  let combined = source : rocks
      gridBounds = getBounds combined
      Boundaries _ _ gridYMin gridYMax = padBounds 2 gridBounds
      Boundaries longXMin longXMax _ _ = padBounds 1000 gridBounds
      longFloor = [(xCoord, gridYMax) | xCoord <- [longXMin .. longXMax]]
      gridCoords =
        [ (xCoord, yCoord)
        | xCoord <- [longXMin .. longXMax]
        , yCoord <- [gridYMin .. gridYMax]
        ]
      canvas = M.fromList $ map (,Air) gridCoords
      grid =
        M.adjust (const Source) source $
          bulkAdjust longFloor (const Rock) $
            bulkAdjust rocks (const Rock) canvas
  return grid

source :: Coordinate
source = point 500 0

--------------------------------------------------------------------------------

elementSymbol :: Element -> Text
elementSymbol Sand = "o"
elementSymbol Rock = "#"
elementSymbol Air = "."
elementSymbol Abyss = "*"
elementSymbol Source = "+"

renderGrid :: Grid Element -> Text
renderGrid = drawGrid Air elementSymbol

--------------------------------------------------------------------------------

isBlocking :: Element -> Bool
isBlocking Sand = True
isBlocking Rock = True
isBlocking _ = False

peekDown :: Coordinate -> Coordinate
peekDown (xCoord, YCoordinate y) = (xCoord, YCoordinate (y + 1))

peekLeftDiagonal :: Coordinate -> Coordinate
peekLeftDiagonal (XCoordinate x, YCoordinate y) =
  (XCoordinate (x - 1), YCoordinate (y + 1))

peekRightDiagonal :: Coordinate -> Coordinate
peekRightDiagonal (XCoordinate x, YCoordinate y) =
  (XCoordinate (x + 1), YCoordinate (y + 1))

dropSand :: Coordinate -> Grid Element -> DropOutcome
dropSand current grid =
  case grid ! downMove of
    Air -> dropSand downMove grid
    Abyss -> FallIntoAbyss
    Source -> undefined
    _ ->
      if not (isBlocking (grid ! ldMove))
        then dropSand ldMove grid
        else
          if not (isBlocking (grid ! rdMove))
            then dropSand rdMove grid
            else
              if current == source
                then BlockSource current
                else LandOnBlocker current
 where
  ldMove = peekLeftDiagonal current
  rdMove = peekRightDiagonal current
  downMove = peekDown current

fillStep :: Grid Element -> Maybe (Grid Element)
fillStep grid =
  case dropSand source grid of
    FallIntoAbyss -> Nothing
    BlockSource _ -> Nothing
    LandOnBlocker c ->
      Just $ M.adjust (const Sand) c grid

fillStepSourceBlock :: Grid Element -> Maybe (Grid Element)
fillStepSourceBlock grid =
  case dropSand source grid of
    FallIntoAbyss -> undefined
    BlockSource _ -> Nothing
    LandOnBlocker c ->
      Just $ M.adjust (const Sand) c grid

fillNStep :: Int -> Grid Element -> Maybe (Grid Element)
fillNStep n grid = foldrM (\_ g -> fillStep g) grid [1 .. n]