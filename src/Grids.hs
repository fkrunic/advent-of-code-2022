module Grids where

import Data.Map (Map)

newtype XCoordinate = XCoordinate Int deriving (Show, Eq, Ord)
newtype YCoordinate = YCoordinate Int deriving (Show, Eq, Ord)
type Coordinate = (XCoordinate, YCoordinate)
type Grid = Map Coordinate

unpackX :: XCoordinate -> Int
unpackX (XCoordinate x) = x

unpackY :: YCoordinate -> Int
unpackY (YCoordinate y) = y

point :: Int -> Int -> Coordinate
point x y = (XCoordinate x, YCoordinate y)