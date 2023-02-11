module Day14 where

import Grids
import Parsing

import Text.Megaparsec

type DrawPath = [Coordinate]
data Element = Sand | Rock | Abyss deriving (Show, Eq) 

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
    builder coord (Just (x:rest)) = (++ rest) <$> pointsAlong coord x

defineGrid :: [DrawPath] -> Grid Element
defineGrid rocks = undefined