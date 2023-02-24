module Problems.Day17 where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Map qualified as M
import Utilities.Grids
import Utilities.Infinites

import Prelude hiding (lookup)

data RockType = HLine | Plus | LShape | VLine | Square
  deriving (Show, Eq, Ord, Enum)

data WindDirection = East | West deriving (Show, Eq, Ord)
data Action = Blow WindDirection | Fall deriving (Show, Eq, Ord)

data Cell = Empty | Surface deriving (Show, Eq)
type Cave = SmartGrid Cell

data InternallyAnchoredShape = InternallyAnchoredShape
  { anchor :: Coordinate
  , remainder :: [Coordinate]
  , bottom :: NonEmpty Coordinate
  }
  deriving (Show, Eq)

data ExternallyAnchoredShape = ExternallyAnchoredShape
  { exAnchor :: Coordinate
  , exShape :: NonEmpty Coordinate
  , exBottom :: NonEmpty Coordinate
  }
  deriving (Show, Eq)

data Shape
  = InternalAnchor InternallyAnchoredShape
  | ExternalAnchor ExternallyAnchoredShape
  deriving (Show, Eq)

newtype RockPosition = RockPosition (NonEmpty Coordinate) deriving (Show, Eq)
newtype RockBottom = RockBottom (NonEmpty Coordinate) deriving (Show, Eq)

data TowerProcess = FallingRock Shape | SettledRock Cave
  deriving (Show, Eq)

newtype Iterations = Iterations Int deriving (Show, Eq, Num)
newtype TowerHeight = TowerHeight Int deriving (Show, Eq, Num)

--------------------------------------------------------------------------------

pWind :: Char -> WindDirection
pWind '>' = East
pWind _ = West

parse :: Text -> [WindDirection]
parse = map pWind . T.unpack

--------------------------------------------------------------------------------

formPosition :: Shape -> RockPosition
formPosition (InternalAnchor ias) = RockPosition (anchor ias :| remainder ias)
formPosition (ExternalAnchor eas) = RockPosition (exShape eas)

shiftShape :: Shape -> Delta -> Shape
shiftShape (InternalAnchor (InternallyAnchoredShape anchor remainder bottom)) d =
  InternalAnchor $
    InternallyAnchoredShape
      { anchor = anchor `shift` d
      , remainder = map (`shift` d) remainder
      , bottom = NE.map (`shift` d) bottom
      }
shiftShape (ExternalAnchor (ExternallyAnchoredShape exAnchor exShape exBottom)) d =
  ExternalAnchor $
    ExternallyAnchoredShape
      { exAnchor = exAnchor `shift` d
      , exShape = NE.map (`shift` d) exShape
      , exBottom = NE.map (`shift` d) exBottom
      }

shiftAnchorTo :: Shape -> Coordinate -> Shape
shiftAnchorTo s@(InternalAnchor ias) desired =
  shiftShape s (desired `diff` anchor ias)
shiftAnchorTo s@(ExternalAnchor eas) desired =
  shiftShape s (desired `diff` exAnchor eas)

typeShape :: RockType -> Shape
typeShape HLine =
  InternalAnchor $
    InternallyAnchoredShape
      { anchor = point 0 0
      , remainder = [point 1 0, point 2 0, point 3 0]
      , bottom = point 0 0 :| [point 1 0, point 2 0, point 3 0]
      }
typeShape Plus =
  ExternalAnchor $
    ExternallyAnchoredShape
      { exAnchor = point 0 0
      , exShape =
          point 1 (-2)
            :| [ point 0 (-1)
               , point 1 (-1)
               , point 2 (-1)
               , point 1 0
               ]
      , exBottom =
          point 0 (-1) :| [point 1 0, point 2 (-1)]
      }
typeShape LShape =
  InternalAnchor $
    InternallyAnchoredShape
      { anchor = point 0 0
      , remainder =
          [ point 1 0
          , point 2 0
          , point 2 (-1)
          , point 2 (-2)
          ]
      , bottom = point 0 0 :| [point 1 0, point 2 0]
      }
typeShape VLine =
  InternalAnchor $
    InternallyAnchoredShape
      { anchor = point 0 0
      , remainder = [point 0 (-1), point 0 (-2), point 0 (-3)]
      , bottom = point 0 0 :| []
      }
typeShape Square =
  InternalAnchor $
    InternallyAnchoredShape
      { anchor = point 0 0
      , remainder = [point 0 (-1), point 1 (-1), point 1 0]
      , bottom = point 0 0 :| [point 1 0]
      }

--------------------------------------------------------------------------------

unpackRockPosition :: RockPosition -> NonEmpty Coordinate
unpackRockPosition (RockPosition ps) = ps

--------------------------------------------------------------------------------

canBlow :: Cave -> WindDirection -> Shape -> Bool
canBlow cave wind shape = not (outsideBounds || isBlocked)
 where
  Boundaries xMin xMax _ _ = gridBounds cave
  RockPosition blown = formPosition $ blowRock wind shape
  outsideBounds =
    any (\xCoord -> xCoord < xMin || xCoord > xMax) $
      NE.map fst blown
  isBlocked = isPositionBlocked cave (RockPosition blown)

isPositionBlocked :: Cave -> RockPosition -> Bool
isPositionBlocked cave (RockPosition ps) = any blocked ps
 where
  blocked b =
    case lookup b cave of
      Nothing -> False
      Just cell -> cell == Surface

canDrop :: Cave -> Shape -> Bool
canDrop cave shape = not $ isPositionBlocked cave (RockPosition shiftedBottom)
 where
  shifted = dropRock shape
  RockBottom shiftedBottom = getBottom shifted

blowRock :: WindDirection -> Shape -> Shape
blowRock East shape = shiftShape shape (DeltaX 1, DeltaY 0)
blowRock West shape = shiftShape shape (DeltaX (-1), DeltaY 0)

dropRock :: Shape -> Shape
dropRock shape = shiftShape shape (DeltaX 0, DeltaY 1)

settleRock :: Cave -> Shape -> Cave
settleRock cave shape =
  foldr (\coord -> insertWith const coord Surface) cave ps
 where
  RockPosition ps = formPosition shape

calculateHeight :: Cave -> TowerHeight
calculateHeight cave =
  case gridBounds cave of
    Boundaries _ _ (YCoordinate yMin) (YCoordinate yMax) ->
      TowerHeight (yMax - yMin)

startingPos :: Cave -> RockType -> Shape
startingPos cave = (`shiftAnchorTo` anchorPoint) . typeShape
 where
  Boundaries xMin _ yMin _ = gridBounds cave
  offset = (DeltaX 2, DeltaY (-4))
  anchorPoint = (xMin, yMin) `shift` offset

getBottom :: Shape -> RockBottom
getBottom (InternalAnchor ias) = RockBottom $ bottom ias
getBottom (ExternalAnchor eas) = RockBottom $ exBottom eas

--------------------------------------------------------------------------------

dropProcess :: Cave -> Shape -> TowerProcess
dropProcess cave shape =
  if canDrop cave shape
    then FallingRock (dropRock shape)
    else SettledRock (settleRock cave shape)

blowProcess :: Cave -> Shape -> WindDirection -> Shape
blowProcess cave shape direction =
  if canBlow cave direction shape
    then blowRock direction shape
    else shape

rockProcess ::
  Cave ->
  Shape ->
  Infinite WindDirection ->
  (Cave, Infinite WindDirection)
rockProcess cave rp winds =
  case dropProcess cave blown of
    FallingRock nextPosition -> rockProcess cave nextPosition futureWinds
    SettledRock settledCave -> (settledCave, futureWinds)
 where
  blown = blowProcess cave rp wind
  (wind, futureWinds) = getSplit winds

towerProcess ::
  Cave ->
  Infinite RockType ->
  Infinite WindDirection ->
  Iterations ->
  Cave
towerProcess cave _ _ 0 = cave
towerProcess cave rts winds iters =
  towerProcess settledCave futureRTS shiftedWinds (iters - 1)
 where
  (rt, futureRTS) = getSplit rts
  (settledCave, shiftedWinds) = rockProcess cave (startingPos cave rt) winds

--------------------------------------------------------------------------------

drawCell :: Cell -> Text
drawCell Empty = "."
drawCell Surface = "#"

drawCave :: Cave -> Text
drawCave = drawGrid Empty drawCell . toGrid

--------------------------------------------------------------------------------

caveFloor :: Cave
caveFloor = makeGrid $ M.fromList $ map (\i -> (point i 0, Surface)) [1 .. 7]

--------------------------------------------------------------------------------

efficientHeight ::
  Cave ->
  NonEmpty RockType ->
  NonEmpty WindDirection ->
  Iterations ->
  TowerHeight
efficientHeight cave rtsSet windSet (Iterations iter)
  | iter < patternMultiple = heightCalc iter
  | otherwise =
      let initialHeight = heightCalc (patternMultiple - 1)
          patternHeight = heightCalc (2 * patternMultiple - 1) - heightCalc patternMultiple
          offset = iter - patternMultiple
          offsetQ = offset `div` patternMultiple
          offsetR = offset `mod` patternMultiple
       in initialHeight + TowerHeight offsetQ * patternHeight + heightCalc offsetR
 where
  patternMultiple = lcm (length rtsSet) (length windSet)
  heightCalc =
    calculateHeight
      . towerProcess caveFloor (makeInf rtsSet) (makeInf windSet)
      . Iterations

--   qHeight + rHeight
--  where
--   uniqueIter = lcm (length rtsSet) (length windSet)
--   heightCalc = calculateHeight
--       . towerProcess caveFloor (makeInf rtsSet) (makeInf windSet)
--       . Iterations
--   quotient = largeIter `div` uniqueIter
--   remainder = largeIter `mod` uniqueIter
--   qHeight = heightCalc uniqueIter * TowerHeight quotient
--   rHeight = heightCalc remainder

-- heights :: [TowerHeight] = map runner [0 .. uniqueStacks]
-- (TowerHeight lastHeight) = last heights
-- remainder = largeIter `mod` uniqueStacks
-- quotient = largeIter `div` uniqueStacks
-- (TowerHeight residueHeight) = heights !! remainder
-- runner :: Int -> TowerHeight
-- runner =
--   calculateHeight
--     . towerProcess caveFloor (makeInf rtsSet) (makeInf windSet)
--     . Iterations
