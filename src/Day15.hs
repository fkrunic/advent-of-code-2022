module Day15 where

import Data.Map.Strict qualified as M
import Data.Text (Text)
import Grids
import Parsing
import Text.Megaparsec.Char (space)

newtype SensorLocation = SensorLocation Coordinate deriving (Show, Eq, Ord)
newtype BeaconLocation = BeaconLocation Coordinate deriving (Show, Eq, Ord)

data SensorBoundary = SensorBoundary
  { northBoundary :: Coordinate
  , southBoundary :: Coordinate
  , eastBoundary :: Coordinate
  , westBoundary :: Coordinate
  }
  deriving (Show, Eq)

newtype ConcreteCells = ConcreteCells [Cell] deriving (Show, Eq)
newtype BoundaryCells = BoundaryCells [Cell] deriving (Show, Eq)

newtype Slope = Slope Int deriving (Show, Eq)
newtype Constant = Constant Int deriving (Show, Eq)
type LineDefinition = (Slope, Constant)

data CellType = Unknown | Sensor | Beacon | Empty deriving (Show, Eq)
type Cell = (Coordinate, CellType)

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

getSensorBoundary :: SensorLocation -> BeaconLocation -> SensorBoundary
getSensorBoundary
  (SensorLocation sLoc)
  (BeaconLocation bLoc) = SensorBoundary{..}
   where
    ManhattanDistance d = manhattanDistance sLoc bLoc
    northBoundary = shift sLoc (DeltaX 0, DeltaY $ negate $ fromIntegral d)
    southBoundary = shift sLoc (DeltaX 0, DeltaY $ fromIntegral d)
    westBoundary = shift sLoc (DeltaX $ negate $ fromIntegral d, DeltaY 0)
    eastBoundary = shift sLoc (DeltaX $ fromIntegral d, DeltaY 0)

isAboveLine :: LineDefinition -> Coordinate -> Bool
isAboveLine (Slope m, Constant b) (XCoordinate x, YCoordinate y) =
  y >= m * x + b

isBelowLine :: LineDefinition -> Coordinate -> Bool
isBelowLine (Slope m, Constant b) (XCoordinate x, YCoordinate y) =
  y <= m * x + b

isInScannerRegion :: (SensorLocation, BeaconLocation) -> Coordinate -> Bool
isInScannerRegion (sensorLoc, beaconLoc) coord =
  and
    [ isBelowLine q1Line coord
    , isBelowLine q2Line coord
    , isAboveLine q3Line coord
    , isAboveLine q4Line coord
    ]
 where
  SensorLocation (XCoordinate _, YCoordinate sy) = sensorLoc
  sbd = getSensorBoundary sensorLoc beaconLoc
  (XCoordinate wbx, YCoordinate _) = westBoundary sbd
  (XCoordinate ebx, YCoordinate _) = eastBoundary sbd

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

combineRegions :: [Coordinate -> Bool] -> Coordinate -> Bool
combineRegions regions coord = any (\inRegion -> inRegion coord) regions

--------------------------------------------------------------------------------

drawCellType :: CellType -> Text
drawCellType Unknown = "."
drawCellType Sensor = "S"
drawCellType Beacon = "B"
drawCellType Empty = "#"

determineCell :: (Coordinate -> Bool) -> Coordinate -> Grid CellType -> CellType
determineCell scanner coord grid =
  case M.lookup coord grid of
    Just Sensor -> Sensor
    Just Beacon -> Beacon
    _ ->
      if scanner coord
        then Empty
        else Unknown

drawCell :: (Coordinate -> Bool) -> Coordinate -> Grid CellType -> Text
drawCell scanner coord grid = drawCellType $ determineCell scanner coord grid

toCells :: (SensorLocation, BeaconLocation) -> (ConcreteCells, BoundaryCells)
toCells (SensorLocation sLoc, BeaconLocation bLoc) =
  (ConcreteCells [(sLoc, Sensor), (bLoc, Beacon)], BoundaryCells boundingCells)
 where
  SensorBoundary north south east west =
    getSensorBoundary (SensorLocation sLoc) (BeaconLocation bLoc)
  boundingCells = map (,Empty) [north, south, east, west]

generateGrid :: [(SensorLocation, BeaconLocation)] -> Grid CellType
generateGrid locs = M.unionWith const concreteGrid boundaryGrid
 where
  concreteGrid =
    M.fromList $
      concatMap (unpackConcreteCells . fst . toCells) locs
  boundaryGrid =
    M.fromList $
      concatMap (unpackBoundaryCells . snd . toCells) locs
  unpackConcreteCells (ConcreteCells cs) = cs
  unpackBoundaryCells (BoundaryCells cs) = cs      

renderField :: [(SensorLocation, BeaconLocation)] -> Text
renderField locs = drawGrid' (drawCell scanner) grid
 where
  scanner = combineRegions $ map isInScannerRegion locs
  grid = generateGrid locs

tuningFrequency :: Coordinate -> Int
tuningFrequency (XCoordinate x, YCoordinate y) = 4000000 * x + y

-- y = mx + b
-- 56000011 = 4000000 * x + y
-- tuningLine :: Coordinate -> LineDefinition
-- tuningLine