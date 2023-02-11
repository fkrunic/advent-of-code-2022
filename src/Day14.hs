module Day14 where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Grids
import Parsing

import Text.Megaparsec

type DrawPath = [Coordinate]
data Element = Sand | Rock | Air | Abyss | Source deriving (Show, Eq)

--------------------------------------------------------------------------------

pVector :: Parser Coordinate
pVector =
  (,)
    <$> (XCoordinate <$> integer <* symbol ",")
    <*> (YCoordinate <$> integer)

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
 where
  source = point 500 0

--------------------------------------------------------------------------------

elementSymbol :: Element -> Text
elementSymbol Sand = "o"
elementSymbol Rock = "#"
elementSymbol Air = "."
elementSymbol Abyss = "*"
elementSymbol Source = "+"

drawGrid :: Grid Element -> Text
drawGrid grid = T.intercalate "\n" rows
 where
  Boundaries xMin xMax yMin yMax = getBounds $ M.keys grid

  drawElement :: Coordinate -> Grid Element -> Text
  drawElement (xc, yc) = elementSymbol . fromMaybe Air . M.lookup (xc, yc)

  rows =
    [ T.intercalate "" row
    | yCoord <- [yMin .. yMax]
    , let row = map (\xc -> drawElement (xc, yCoord) grid) [xMin .. xMax]
    ]