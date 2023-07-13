module Problems.Day15 where

import Data.Map (Map, (!))
import Data.Map.Strict qualified as M
import Data.Maybe (isJust)
import Data.Text (Text)
import Utilities.Grids
import Utilities.Parsing
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

newtype ConcreteCells = ConcreteCells [Cell] deriving (Show, Eq)
newtype BoundaryCells = BoundaryCells [Cell] deriving (Show, Eq)

newtype Slope = Slope Int deriving (Show, Eq, Ord)
newtype Constant = Constant Int deriving (Show, Eq, Ord)
type LineDefinition = (Slope, Constant)

data CellType = Unknown | Sensor | Beacon | Empty | Marker deriving (Show, Eq)
type Cell = (Coordinate, CellType)

type ScannerAssoc = Map Scanner SensorID
type SensorLimits = Map SensorID SensorBoundary

data Quadrant = Q1 | Q2 | Q3 | Q4 deriving (Show, Eq, Ord)

data SearchError
  = UnableToDetermineQuadrant SensorID Coordinate
  | ExhaustedSearchSpace
  deriving (Show, Eq)

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
    if x2 - x1 == 0
      then undefined
      else Slope $ (y2 - y1) `div` (x2 - x1)

deduceConstant :: Slope -> Coordinate -> Constant
deduceConstant (Slope m) (XCoordinate bx, YCoordinate by) =
  Constant $ by - m * bx

buildLine :: Coordinate -> Coordinate -> LineDefinition
buildLine boundary1 boundary2 = (slope, constant)
 where
  slope = riseOverRun boundary1 boundary2
  constant = deduceConstant slope boundary1

{-           North
               y-
               |
              .|.
             . | . (lineAbove)
            .  |  .
           . Q3|Q4 .
 West (x-)-----S-----(x+) East
           . Q1|Q2 .
            .  |  .
             . | . (lineBelow)
              .|.
               |
               y+
             South
-}

makeScanner :: (SensorLocation, BeaconLocation) -> Scanner
makeScanner (sensorLoc, beaconLoc) = Scanner{..}
 where
  sbd = getSensorBoundary sensorLoc beaconLoc
  q1Line = buildLine (westBoundary sbd) (southBoundary sbd)
  q2Line = buildLine (southBoundary sbd) (eastBoundary sbd)
  q3Line = buildLine (westBoundary sbd) (northBoundary sbd)
  q4Line = buildLine (northBoundary sbd) (eastBoundary sbd)

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

teleportAcrossQ :: Quadrant -> Quadrant
teleportAcrossQ Q1 = Q2
teleportAcrossQ Q3 = Q4
teleportAcrossQ Q4 = Q4
teleportAcrossQ Q2 = Q2

getLineFromQ :: Quadrant -> Scanner -> LineDefinition
getLineFromQ Q1 = q1Line
getLineFromQ Q2 = q2Line
getLineFromQ Q3 = q3Line
getLineFromQ Q4 = q4Line

deduceBoundaryPosition :: YCoordinate -> LineDefinition -> Coordinate
deduceBoundaryPosition yCoord@(YCoordinate y) (Slope m, Constant b) =
  (XCoordinate $ (y - b) `div` m, yCoord)

{-           North
               y-
               |
              .|.
             . | . (lineAbove)
            .  |  .
           . Q3|Q4 .
 West (x-)-----S-----(x+) East
           . Q1|Q2 .
            .  |  .
             . | . (lineBelow)
              .|.
               |
               y+
             South
-}

isWestOf :: XCoordinate -> Coordinate -> Bool
isWestOf (XCoordinate mark) (XCoordinate x, _) = x <= mark

isEastOf :: XCoordinate -> Coordinate -> Bool
isEastOf (XCoordinate mark) (XCoordinate x, _) = x >= mark

isNorthOf :: YCoordinate -> Coordinate -> Bool
isNorthOf (YCoordinate mark) (_, YCoordinate y) = y <= mark

isSouthOf :: YCoordinate -> Coordinate -> Bool
isSouthOf (YCoordinate mark) (_, YCoordinate y) = y >= mark

deduceCurrentQuadrant ::
  Coordinate ->
  SensorLocation ->
  Scanner ->
  Maybe Quadrant
deduceCurrentQuadrant
  coord
  (SensorLocation (sx, sy))
  (Scanner q1Line q2Line q3Line q4Line)
    | isBelowLine q1Line coord && isWestOf sx coord && isSouthOf sy coord =
        Just Q1
    | isBelowLine q2Line coord && isEastOf sx coord && isSouthOf sy coord =
        Just Q2
    | isAboveLine q3Line coord && isWestOf sx coord && isNorthOf sy coord =
        Just Q3
    | isAboveLine q4Line coord && isEastOf sx coord && isNorthOf sy coord =
        Just Q4
    | otherwise = Nothing

teleportAcrossSensor ::
  Coordinate ->
  SensorLocation ->
  Scanner ->
  Maybe Coordinate
teleportAcrossSensor coord@(_, yCoord) sensorLoc scanner = do
  currentQ <- deduceCurrentQuadrant coord sensorLoc scanner
  let crossQ = teleportAcrossQ currentQ
      crossBoundary = getLineFromQ crossQ scanner
  return $ deduceBoundaryPosition yCoord crossBoundary

stepCoordinate :: Coordinate -> Boundaries -> Coordinate
stepCoordinate (xCoord@(XCoordinate x), YCoordinate y) bounds
  | xCoord > xMax = (xMin, YCoordinate (y + 1))
  | otherwise = (XCoordinate (x + 1), YCoordinate y)
 where
  Boundaries xMin xMax _ _ = bounds

findDistressBeacon ::
  Map SensorID (SensorLocation, Scanner) ->
  [(SensorID, Scanner)] ->
  Boundaries ->
  Coordinate ->
  Either SearchError Coordinate
findDistressBeacon sensorDetails scanners bounds current =
  case activateAll scanners current of
    Nothing -> Right current
    Just sid ->
      let (sensorLoc, scanner) = sensorDetails ! sid
       in case teleportAcrossSensor current sensorLoc scanner of
            Nothing -> Left $ UnableToDetermineQuadrant sid current
            Just teleported ->
              let next@(nxLoc, nyLoc) = stepCoordinate teleported bounds
               in if nxLoc >= xMax && nyLoc >= yMax
                    then Left ExhaustedSearchSpace
                    else findDistressBeacon sensorDetails scanners bounds next
 where
  Boundaries _ xMax _ yMax = bounds

distress ::
  [SensorPair] ->
  Boundaries ->
  Either SearchError Coordinate
distress pairs bounds =
  findDistressBeacon sensorDetails scanners bounds (point 0 0)
 where
  sensorDetails = M.fromList $ map buildDetail pairs
  scanners = map labelScanners pairs
  buildDetail sp =
    (sensorID sp, (fst $ sensorPair sp, makeScanner (sensorPair sp)))
  labelScanners sp = (sensorID sp, makeScanner (sensorPair sp))
