module Day14 where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)

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
      xMin = minimum $ map (unpackX . fst) combined
      xMax = maximum $ map (unpackX . fst) combined
      yMin = minimum $ map (unpackY . snd) combined
      yMax = maximum $ map (unpackY . snd) combined
      gridXMin = xMin - 1
      gridXMax = xMax + 1
      gridYMax = yMax + 1
      abyssRow = [point x gridYMax | x <- [gridXMin .. gridXMax]]
      gridCoords = [point x y | x <- [xMin .. xMax], y <- [yMin .. yMax]]
      canvas = M.fromList $ map (,Air) gridCoords
      grid =
        bulkAdjust abyssRow (const Abyss) $
          bulkAdjust rocks (const Rock) canvas
  return $ M.adjust (const Source) source grid
 where
  source = point 500 0

--------------------------------------------------------------------------------  

drawElement :: Element -> Text
drawElement Sand = "o"
drawElement Rock = "#"
drawElement Air = "."
drawElement Abyss = "*"
drawElement Source = "+"