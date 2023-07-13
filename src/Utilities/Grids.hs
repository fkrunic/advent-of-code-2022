{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Utilities.Grids where

import Data.List (unfoldr)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

newtype XCoordinate = XCoordinate Int deriving (Show, Eq, Ord, Enum, Num)
newtype YCoordinate = YCoordinate Int deriving (Show, Eq, Ord, Enum, Num)

newtype ManhattanDistance = ManhattanDistance Word deriving (Show, Eq, Ord)

newtype DeltaX = DeltaX Int deriving (Show, Eq, Ord)
newtype DeltaY = DeltaY Int deriving (Show, Eq, Ord)
type Delta = (DeltaX, DeltaY)

type Coordinate = (XCoordinate, YCoordinate)
type Grid = Map Coordinate

data SmartGrid a = SmartGrid (Map Coordinate a) Boundaries
  deriving (Show, Eq)

data Boundaries = Boundaries
  { xMin :: XCoordinate
  , xMax :: XCoordinate
  , yMin :: YCoordinate
  , yMax :: YCoordinate
  }
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------

makeGrid :: Map Coordinate a -> SmartGrid a 
makeGrid m = SmartGrid m (getBounds $ M.keys m)

updateBounds :: Coordinate -> Boundaries -> Boundaries
updateBounds (xCoord, yCoord) (Boundaries xMin xMax yMin yMax) =
  Boundaries 
    { xMin = min xCoord xMin
    , xMax = max xCoord xMax
    , yMin = min yCoord yMin
    , yMax = max yCoord yMax
    }

insert :: Coordinate -> a -> SmartGrid a -> SmartGrid a
insert coord v (SmartGrid m bounds) = 
  SmartGrid (M.insert coord v m) (updateBounds coord bounds)

insertWith :: (a -> a -> a) -> Coordinate -> a -> SmartGrid a -> SmartGrid a
insertWith combiner coord v (SmartGrid m bounds) = 
  SmartGrid (M.insertWith combiner coord v m) (updateBounds coord bounds)

gridBounds :: SmartGrid a -> Boundaries
gridBounds (SmartGrid _ bounds) = bounds

lookup :: Coordinate -> SmartGrid a -> Maybe a
lookup coord (SmartGrid m _) = M.lookup coord m

toGrid :: SmartGrid a -> Grid a 
toGrid (SmartGrid m _) = m

--------------------------------------------------------------------------------

invert :: Delta -> Delta
invert (DeltaX dx, DeltaY dy) = (DeltaX (negate dx), DeltaY (negate dy))

diff :: Coordinate -> Coordinate -> Delta
diff (x1, y1) (x2, y2) =
  (DeltaX $ unpackX $ x1 - x2, DeltaY $ unpackY $ y1 - y2)

delta :: Coordinate -> Delta -> Coordinate
delta (XCoordinate x, YCoordinate y) (DeltaX dx, DeltaY dy) =
  (XCoordinate (x + dx), YCoordinate (y + dy))

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

drawGrid' :: (Coordinate -> Grid a -> Text) -> Grid a -> Text
drawGrid' drawElement grid = T.intercalate "\n" rows
 where
  Boundaries xMin xMax yMin yMax = getBounds $ M.keys grid
  rows =
    [ T.intercalate "" row
    | yCoord <- [yMin .. yMax]
    , let row = map (\xc -> drawElement (xc, yCoord) grid) [xMin .. xMax]
    ]

manhattanDistance :: Coordinate -> Coordinate -> ManhattanDistance
manhattanDistance
  (XCoordinate x1, YCoordinate y1)
  (XCoordinate x2, YCoordinate y2) =
    ManhattanDistance $ fromIntegral $ abs (x1 - x2) + abs (y1 - y2)

shift :: Coordinate -> Delta -> Coordinate
shift (XCoordinate x, YCoordinate y) (DeltaX dx, DeltaY dy) =
  (XCoordinate (x + dx), YCoordinate (y + dy))
