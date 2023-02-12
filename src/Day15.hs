module Day15 where

import Data.Text (Text)
import Grids
import Parsing
import Text.Megaparsec.Char (space)

newtype SensorLocation = SensorLocation Coordinate deriving (Show, Eq, Ord)
newtype BeaconLocation = BeaconLocation Coordinate deriving (Show, Eq, Ord)

newtype Slope = Slope Int deriving (Show, Eq)
newtype Constant = Constant Int deriving (Show, Eq)
type LineDefinition = (Slope, Constant)

data CellType = Unknown | Sensor | Beacon | Empty deriving (Show, Eq)

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

isAboveLine :: LineDefinition -> Coordinate -> Bool
isAboveLine (Slope m, Constant b) (XCoordinate x, YCoordinate y) =
  y >= m * x + b

isBelowLine :: LineDefinition -> Coordinate -> Bool
isBelowLine (Slope m, Constant b) (XCoordinate x, YCoordinate y) =
  y <= m * x + b

-- scannedRegion :: (SensorLocation, BeaconLocation) -> [Coordinate]
-- scannedRegion (SensorLocation sLoc, BeaconLocation bLoc) = [sLoc]
--   where
--     ManhattanDistance d = manhattanDistance sLoc bLoc
--     northBoundary = shift sLoc (DeltaX 0, DeltaY $ negate $ fromIntegral d)
--     southBoundary = shift sLoc (DeltaX 0, DeltaY $ fromIntegral d)
--     westBoundary = shift sLoc (DeltaX $ negate $ fromIntegral d, DeltaY 0)
--     eastBoundary = shift sLoc (DeltaX $ fromIntegral d, DeltaY 0)

--     -- y = sLoc@y, x = westBoundary@x
--     -- y = mx + b
--     -- y = x + b
--     -- sLoc@y = westBoundary@x + b
--     -- b = sLoc@y - westBoundary@x
--     -- (m=1, b=sLoc@y - westBoundary@x)
--     q1 (xCoord, yCoord) =

--------------------------------------------------------------------------------

drawCellType :: CellType -> Text
drawCellType Unknown = "."
drawCellType Sensor = "S"
drawCellType Beacon = "B"
drawCellType Empty = "#"