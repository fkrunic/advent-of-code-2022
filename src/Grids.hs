{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grids where

import Data.List (unfoldr)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

newtype XCoordinate = XCoordinate Int deriving (Show, Eq, Ord, Enum)
newtype YCoordinate = YCoordinate Int deriving (Show, Eq, Ord, Enum)
type Coordinate = (XCoordinate, YCoordinate)
type Grid = Map Coordinate

data Boundaries = Boundaries
  { xMin :: XCoordinate
  , xMax :: XCoordinate
  , yMin :: YCoordinate
  , yMax :: YCoordinate
  }
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------

unpackX :: XCoordinate -> Int
unpackX (XCoordinate x) = x

unpackY :: YCoordinate -> Int
unpackY (YCoordinate y) = y

point :: Int -> Int -> Coordinate
point x y = (XCoordinate x, YCoordinate y)

getBounds :: [Coordinate] -> Boundaries
getBounds xs = Boundaries{..}
 where
  xMin = minimum $ map fst xs
  xMax = maximum $ map fst xs
  yMin = minimum $ map snd xs
  yMax = maximum $ map snd xs

padBounds :: Int -> Boundaries -> Boundaries
padBounds padding bounds =
  Boundaries
    { xMin = XCoordinate (xLower - padding)
    , xMax = XCoordinate (xUpper + padding)
    , yMin = YCoordinate (yLower - padding)
    , yMax = YCoordinate (yUpper + padding)
    }
 where
  (XCoordinate xLower) = xMin bounds
  (XCoordinate xUpper) = xMax bounds
  (YCoordinate yLower) = yMin bounds
  (YCoordinate yUpper) = yMax bounds

moveNorth :: Coordinate -> Coordinate
moveNorth (xCoord, YCoordinate y) = (xCoord, YCoordinate (y - 1))

moveSouth :: Coordinate -> Coordinate
moveSouth (xCoord, YCoordinate y) = (xCoord, YCoordinate (y + 1))

moveWest :: Coordinate -> Coordinate
moveWest (XCoordinate x, yCoord) = (XCoordinate (x - 1), yCoord)

moveEast :: Coordinate -> Coordinate
moveEast (XCoordinate x, yCoord) = (XCoordinate (x + 1), yCoord)

generateDirectionalView ::
  Grid a ->
  (Coordinate -> Coordinate) ->
  Coordinate ->
  [(Coordinate, a)]
generateDirectionalView grid mover = tail . unfoldr scanner
 where
  scanner coord = do
    v <- M.lookup coord grid
    Just ((coord, v), mover coord)

drawGrid :: a -> (a -> Text) -> Grid a -> Text
drawGrid defaultElement elementSymbol grid = T.intercalate "\n" rows
 where
  Boundaries xMin xMax yMin yMax = getBounds $ M.keys grid

  drawElement (xc, yc) = 
    elementSymbol . fromMaybe defaultElement . M.lookup (xc, yc)

  rows =
    [ T.intercalate "" row
    | yCoord <- [yMin .. yMax]
    , let row = map (\xc -> drawElement (xc, yCoord) grid) [xMin .. xMax]
    ]