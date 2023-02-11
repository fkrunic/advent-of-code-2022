{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grids where

import Data.Map (Map)

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
