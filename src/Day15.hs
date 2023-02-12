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

isInScannerRegion :: (SensorLocation, BeaconLocation) -> Coordinate -> Bool
isInScannerRegion
  (SensorLocation sLoc@(XCoordinate _, YCoordinate sy), BeaconLocation bLoc)
  coord =
    and
      [ isBelowLine q1Line coord
      , isBelowLine q2Line coord
      , isAboveLine q3Line coord
      , isAboveLine q4Line coord
      ]
   where
    ManhattanDistance d = manhattanDistance sLoc bLoc

    (XCoordinate wbx, YCoordinate _) =
      shift sLoc (DeltaX $ negate $ fromIntegral d, DeltaY 0)

    (XCoordinate ebx, YCoordinate _) =
      shift sLoc (DeltaX $ fromIntegral d, DeltaY 0)

    -- y = sLoc@y, x = westBoundary@x
    -- y = mx + b
    -- y = x + b
    -- sLoc@y = westBoundary@x + b
    -- b = sLoc@y - westBoundary@x
    -- (m=1, b=sLoc@y - westBoundary@x)
    q1Line = (Slope 1, Constant $ sy - wbx)

    -- y = sLoc@y, x = eastBoundary@x
    -- y = mx + b
    -- y = -x + b
    q2Line = (Slope (-1), Constant $ sy + ebx)

    -- y = sLoc@y, x = westBoundary@x
    q3Line = (Slope (-1), Constant $ sy + wbx)

    -- y = sLoc@y, x = eastBoundary@x
    q4Line = (Slope 1, Constant $ sy - ebx)

combineRegions :: Coordinate -> [Coordinate -> Bool] -> Bool
combineRegions coord = any (\inRegion -> inRegion coord)

--------------------------------------------------------------------------------

drawCellType :: CellType -> Text
drawCellType Unknown = "."
drawCellType Sensor = "S"
drawCellType Beacon = "B"
drawCellType Empty = "#"