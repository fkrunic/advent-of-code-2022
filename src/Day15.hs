module Day15 where

import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (isJust)
import Data.Text (Text)
import Grids
import Parsing
import Text.Megaparsec
import Text.Megaparsec.Char (space)

newtype SensorID = SensorID Int deriving (Show, Eq, Ord)
newtype SensorLocation = SensorLocation Coordinate deriving (Show, Eq, Ord)
newtype BeaconLocation = BeaconLocation Coordinate deriving (Show, Eq, Ord)
newtype MarkerLocation = MarkerLocation Coordinate deriving (Show, Eq, Ord)

data SensorPair = SensorPair
  { sensorID :: SensorID
  , sensorPair :: (SensorLocation, BeaconLocation)
  }
  deriving (Show, Eq)

data SensorBoundary = SensorBoundary
  { northBoundary :: Coordinate
  , southBoundary :: Coordinate
  , eastBoundary :: Coordinate
  , westBoundary :: Coordinate
  }
  deriving (Show, Eq)

data LocationLayout = LocationLayout
  { sensorPairs :: [SensorPair]
  , markerLoc :: Maybe MarkerLocation
  }
  deriving (Show, Eq)

data Scanner = Scanner
  { q1Line :: LineDefinition
  , q2Line :: LineDefinition
  , q3Line :: LineDefinition
  , q4Line :: LineDefinition
  }
  deriving (Show, Eq, Ord)

newtype ConcreteCells = ConcreteCells [Cell] deriving (Show, Eq)
newtype BoundaryCells = BoundaryCells [Cell] deriving (Show, Eq)

newtype Slope = Slope Int deriving (Show, Eq, Ord)
newtype Constant = Constant Int deriving (Show, Eq, Ord)
type LineDefinition = (Slope, Constant)

data CellType = Unknown | Sensor | Beacon | Empty | Marker deriving (Show, Eq)
type Cell = (Coordinate, CellType)

type ScannerAssoc = Map Scanner SensorID
type SensorLimits = Map SensorID SensorBoundary

data Field = Field
  { cellGrid :: Grid Cell
  , scannerAssoc :: ScannerAssoc
  , sensorLimits :: SensorLimits
  }
  deriving (Show, Eq)

data Quadrant = Q1 | Q2 | Q3 | Q4 deriving (Show, Eq, Ord)

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

pSensors :: Parser [SensorPair]
pSensors = zipWith (SensorPair . SensorID) [1 ..] <$> some pLine

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

riseOverRun :: Coordinate -> Coordinate -> Slope
riseOverRun
  (XCoordinate x1, YCoordinate y1)
  (XCoordinate x2, YCoordinate y2) =
    Slope $ (y2 - y1) `div` (x2 - x1)

activate :: SensorID -> Scanner -> Coordinate -> Maybe SensorID
activate sid (Scanner q1Line q2Line q3Line q4Line) coord =
  if and
    [ isBelowLine q1Line coord
    , isBelowLine q2Line coord
    , isAboveLine q3Line coord
    , isAboveLine q4Line coord
    ]
    then Just sid
    else Nothing

makeScanner :: (SensorLocation, BeaconLocation) -> Scanner
makeScanner (sensorLoc, beaconLoc) = Scanner{..}
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

activateAll :: [(SensorID, Scanner)] -> Coordinate -> Maybe SensorID
activateAll [] _ = Nothing
activateAll ((sid, scanner) : scs) coord =
  case activate sid scanner coord of
    Just tag -> Just tag
    Nothing -> activateAll scs coord

--------------------------------------------------------------------------------

drawCellType :: CellType -> Text
drawCellType Unknown = "."
drawCellType Sensor = "S"
drawCellType Beacon = "B"
drawCellType Empty = "#"
drawCellType Marker = "X"

determineCell :: (Coordinate -> Bool) -> Coordinate -> Grid CellType -> CellType
determineCell scanner coord grid =
  case M.lookup coord grid of
    Just Marker -> Marker
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

generateGrid :: LocationLayout -> Grid CellType
generateGrid (LocationLayout locs ml) =
  case ml of
    Nothing -> merged
    Just (MarkerLocation mkLoc) -> M.insertWith const mkLoc Marker merged
 where
  concreteGrid =
    M.fromList $
      concatMap (unpackConcreteCells . fst . toCells . sensorPair) locs
  boundaryGrid =
    M.fromList $
      concatMap (unpackBoundaryCells . snd . toCells . sensorPair) locs
  merged = M.unionWith const concreteGrid boundaryGrid
  unpackConcreteCells (ConcreteCells cs) = cs
  unpackBoundaryCells (BoundaryCells cs) = cs

renderField :: LocationLayout -> Text
renderField layout = drawGrid' (drawCell (isJust . scanner)) grid
 where
  scanner =
    activateAll $
      map (\sp -> (sensorID sp, makeScanner (sensorPair sp))) $
        sensorPairs layout
  grid = generateGrid layout

--------------------------------------------------------------------------------

tuningFrequency :: Coordinate -> Int
tuningFrequency (XCoordinate x, YCoordinate y) = 4000000 * x + y

reflectAcrossSensor :: SensorLocation -> Coordinate -> Coordinate
reflectAcrossSensor
  (SensorLocation (XCoordinate sx, _))
  (XCoordinate cx, yCoord) = (XCoordinate x', yCoord)
   where
    dx = sx - cx
    x' = cx + 2 * dx

reflect :: Quadrant -> Quadrant
reflect Q1 = Q2
reflect Q2 = Q1
reflect Q3 = Q4
reflect Q4 = Q3

getLineFromQ :: Quadrant -> Scanner -> LineDefinition
getLineFromQ Q1 = q1Line
getLineFromQ Q2 = q2Line
getLineFromQ Q3 = q3Line
getLineFromQ Q4 = q4Line

deduceBoundaryPosition :: YCoordinate -> LineDefinition -> Coordinate
deduceBoundaryPosition yCoord@(YCoordinate y) (Slope m, Constant b) =
  (XCoordinate $ (y - b) `div` m, yCoord)

deduceCurrentQuadrant ::
  Coordinate ->
  SensorLocation ->
  Scanner ->
  Maybe Quadrant
deduceCurrentQuadrant
  coord@(XCoordinate cx, YCoordinate cy)
  (SensorLocation (XCoordinate sx, YCoordinate sy))
  scanner
    | isBelowLine (q1Line scanner) coord && cx <= sx && cy <= sy = Just Q1
    | isBelowLine (q2Line scanner) coord && cx >= sx && cy <= sy = Just Q2
    | isAboveLine (q3Line scanner) coord && cx <= sx && cy >= sy = Just Q3
    | isAboveLine (q4Line scanner) coord && cx >= sx && cy >= sy = Just Q4
    | otherwise = Nothing

teleportAcrossSensor ::
  Coordinate ->
  SensorLocation ->
  Scanner ->
  Maybe Coordinate
teleportAcrossSensor coord@(_, yCoord) sensorLoc scanner = do
  currentQ <- deduceCurrentQuadrant coord sensorLoc scanner
  let crossQ = reflect currentQ
      crossBoundary = getLineFromQ crossQ scanner
  return $ deduceBoundaryPosition yCoord crossBoundary

-- y = mx + b
-- y - b = mx
-- x = (y - b) / m