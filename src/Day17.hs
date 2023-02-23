module Day17 where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)

import Data.Map qualified as M

import Grids

import Data.Text qualified as T
import Infinites

data RockType = HLine | Plus | LShape | VLine | Square
  deriving (Show, Eq, Ord, Enum)

data WindDirection = East | West deriving (Show, Eq, Ord)
data Action = Blow WindDirection | Fall deriving (Show, Eq, Ord)

data Cell = Empty | Surface deriving (Show, Eq)
type Cave = Grid Cell

data InternallyAnchoredShape = InternallyAnchoredShape
  { anchor :: Coordinate
  , remainder :: [Coordinate]
  }
  deriving (Show, Eq)

data ExternallyAnchoredShape = ExternallyAnchoredShape
  { exAnchor :: Coordinate
  , exShape :: NonEmpty Coordinate
  }
  deriving (Show, Eq)

data Shape
  = InternalAnchor InternallyAnchoredShape
  | ExternalAnchor ExternallyAnchoredShape
  deriving (Show, Eq)

newtype RockPosition = RockPosition (NonEmpty Coordinate) deriving (Show, Eq)
newtype RockBottom = RockBottom (NonEmpty Coordinate) deriving (Show, Eq)

data TowerProcess = FallingRock RockPosition | SettledRock Cave
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
shiftShape (InternalAnchor (InternallyAnchoredShape anchor remainder)) d =
  InternalAnchor $
    InternallyAnchoredShape
      { anchor = anchor `shift` d
      , remainder = map (`shift` d) remainder
      }
shiftShape (ExternalAnchor (ExternallyAnchoredShape exAnchor exShape)) d =
  ExternalAnchor $
    ExternallyAnchoredShape
      { exAnchor = exAnchor `shift` d
      , exShape = NE.map (`shift` d) exShape
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
               , point 0 1
               ]
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
      }
typeShape VLine =
  InternalAnchor $
    InternallyAnchoredShape
      { anchor = point 0 0
      , remainder = [point 0 (-1), point 0 (-2), point 0 (-3)]
      }
typeShape Square =
  InternalAnchor $
    InternallyAnchoredShape
      { anchor = point 0 0
      , remainder = [point 0 (-1), point 1 (-1), point 1 0]
      }

--------------------------------------------------------------------------------

unpackRockPosition :: RockPosition -> NonEmpty Coordinate
unpackRockPosition (RockPosition ps) = ps

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
settleRock cave (RockPosition ps) =
  foldr (\coord -> M.insertWith const coord Surface) cave ps

calculateHeight :: Cave -> TowerHeight
calculateHeight cave =
  case getBounds (M.keys cave) of
    Boundaries _ _ (YCoordinate yMin) (YCoordinate yMax) ->
      TowerHeight (yMax - yMin)

startingPos :: Cave -> RockType -> RockPosition
startingPos cave = formPosition . (`shiftAnchorTo` anchorPoint) . typeShape
 where
  Boundaries xMin _ yMin _ = getBounds $ M.keys cave
  offset = (DeltaX 3, DeltaY (-4))
  anchorPoint = (xMin, yMin) `shift` offset

getBottom :: RockPosition -> RockBottom
getBottom =
  RockBottom
    . NE.map (NE.head . NE.reverse . NE.sortWith snd)
    . NE.groupBy1 (\c1 c2 -> fst c1 == fst c2)
    . unpackRockPosition

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
drawCave = drawGrid Empty drawCell

--------------------------------------------------------------------------------

caveFloor :: Cave
caveFloor = M.fromList $ map (\i -> (point i 0, Surface)) [1 .. 7]
