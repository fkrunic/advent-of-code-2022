module Day17 where

import Data.List

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE

import Data.Map qualified as M

import Grids

import Infinites

data RockType = HLine | Plus | LShape | VLine | Square
  deriving (Show, Eq, Ord, Enum)

data WindDirection = East | West deriving (Show, Eq, Ord)
data Action = Blow WindDirection | Fall deriving (Show, Eq, Ord)

data Cell = Empty | Surface deriving (Show, Eq)
type Cave = Grid Cell

newtype RockPosition = RockPosition (NonEmpty Coordinate) deriving (Show, Eq)
newtype RockBottom = RockBottom (NonEmpty Coordinate) deriving (Show, Eq)

data TowerProcess = FallingRock RockPosition | SettledRock Cave
  deriving (Show, Eq)

newtype Iterations = Iterations Int deriving (Show, Eq, Num)
newtype TowerHeight = TowerHeight Int deriving (Show, Eq, Num)

--------------------------------------------------------------------------------

canBlow :: Cave -> WindDirection -> RockPosition -> Bool
canBlow cave wind rp = not (outsideBounds || isBlocked)
 where
  Boundaries xMin xMax _ _ = getBounds $ M.keys cave
  RockPosition blown = blowRock wind rp
  outsideBounds =
    any (\xCoord -> xCoord < xMin || xCoord > xMax) $
      NE.map fst blown
  isBlocked = isPositionBlocked cave (RockPosition blown)

isPositionBlocked :: Cave -> RockPosition -> Bool
isPositionBlocked cave (RockPosition ps) = any blocked ps
 where
  blocked b =
    case M.lookup b cave of
      Nothing -> False
      Just cell -> cell == Surface

canDrop :: Cave -> RockBottom -> Bool
canDrop cave (RockBottom bs) = not $ isPositionBlocked cave shifted
 where
  shifted = dropRock (RockPosition bs)

blowRock :: WindDirection -> RockPosition -> RockPosition
blowRock East (RockPosition ps) = RockPosition $ NE.map moveEast ps
blowRock West (RockPosition ps) = RockPosition $ NE.map moveWest ps

dropRock :: RockPosition -> RockPosition
dropRock (RockPosition ps) = RockPosition $ NE.map moveSouth ps

settleRock :: Cave -> RockPosition -> Cave
settleRock cave (RockPosition ps) = foldr (M.adjust (const Surface)) cave ps

calculateHeight :: Cave -> TowerHeight
calculateHeight cave =
  case getBounds (M.keys cave) of
    Boundaries _ _ (YCoordinate yMin) (YCoordinate yMax) ->
      TowerHeight (yMax - yMin)

startingPos :: Cave -> RockType -> RockPosition
startingPos = undefined

getBottom :: RockPosition -> RockBottom
getBottom = undefined

--------------------------------------------------------------------------------

dropProcess :: Cave -> RockPosition -> TowerProcess
dropProcess cave rp =
  if canDrop cave bottom
    then FallingRock (dropRock rp)
    else SettledRock (settleRock cave rp)
 where
  bottom = getBottom rp

blowProcess :: Cave -> RockPosition -> WindDirection -> RockPosition
blowProcess cave rp direction =
  if canBlow cave direction rp
    then blowRock direction rp
    else rp

rockProcess ::
  Cave ->
  RockPosition ->
  Infinite WindDirection ->
  (Cave, Infinite WindDirection)
rockProcess cave rp winds =
  case dropProcess cave blown of
    FallingRock rp'' -> rockProcess cave rp'' futureWinds
    SettledRock cave' -> (cave', futureWinds)
 where
  blown = blowProcess cave rp wind
  (wind, futureWinds) = getSplit winds

towerProcess ::
  Cave ->
  Infinite RockType ->
  Infinite WindDirection ->
  Iterations ->
  TowerHeight
towerProcess cave _ _ 0 = calculateHeight cave
towerProcess cave rts winds iters =
  towerProcess settledCave futureRTS shiftedWinds (iters - 1)
 where
  (rt, futureRTS) = getSplit rts
  (settledCave, shiftedWinds) = rockProcess cave (startingPos cave rt) winds
