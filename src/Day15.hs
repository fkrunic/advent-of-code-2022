module Day15 where

import Grids
import Parsing
import Text.Megaparsec.Char (space)

newtype SensorLocation = SensorLocation Coordinate deriving (Show, Eq, Ord)
newtype BeaconLocation = BeaconLocation Coordinate deriving (Show, Eq, Ord)

pCoord :: Parser Coordinate
pCoord =
  (,)
    <$> (XCoordinate <$> (symbol "x=" *> signedInteger <* symbol ","))
    <*> (YCoordinate <$> (symbol "y=" *> signedInteger))

pLine :: Parser (SensorLocation, BeaconLocation)
pLine =
  (,)
    <$> (SensorLocation <$> (symbol "Sensor at" *> pCoord <* symbol ": "))
    <*> (BeaconLocation <$> (symbol "closest beacon is at" *> pCoord <* space))