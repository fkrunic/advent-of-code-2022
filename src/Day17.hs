module Day17 where

import Data.List

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE

import Grids

-- data RockType = HLine | Plus | LShape | VLine | Square
--   deriving (Show, Eq, Ord, Enum)

data WindDirection = East | West deriving (Show, Eq, Ord)
data Action = Blow WindDirection | Fall deriving (Show, Eq, Ord)

-- newtype CaveBoundaries = CaveBoundaries Boundaries deriving (Show, Eq)
data Cell = Empty | Surface deriving (Show, Eq)
type Cave = Grid Cell

newtype RockPosition = RockPosition (NonEmpty Coordinate) deriving (Show, Eq)
newtype RockBottom = RockBottom (NonEmpty Coordinate) deriving (Show, Eq)
-- type Rock = (RockType, RockPosition)

-- newtype RocksDropped = RocksDropped Int deriving (Show, Eq, Num)
-- newtype TowerHeight = TowerHeight Int deriving (Show, Eq)

-- data TowerProcess = TowerProcess
--   { cave :: Cave
--   , rocksDropped :: Int
--   , towerHeight :: Int
--   }
--   deriving (Show, Eq)

data TowerProcess = FallingRock RockPosition | SettledRock Cave

newtype Infinite a = Infinite (NonEmpty a)

getNext :: Infinite a -> a
getNext = undefined

getRest :: Infinite a -> Infinite a 
getRest = undefined

--------------------------------------------------------------------------------

canBlow :: Cave -> WindDirection -> RockPosition -> Bool
canBlow = undefined

canDrop :: Cave -> RockBottom -> Bool
canDrop = undefined

blowRock :: Cave -> WindDirection -> RockPosition -> RockPosition
blowRock = undefined

dropRock :: RockPosition -> RockPosition
dropRock = undefined

settleRock :: Cave -> RockPosition -> Cave
settleRock = undefined

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

blowProcess :: Cave -> RockPosition -> WindDirection -> TowerProcess
blowProcess cave rp direction = 
  if canBlow cave direction rp
    then FallingRock (blowRock cave direction rp)
    else FallingRock rp

rockProcess :: Cave -> RockPosition -> Infinite WindDirection -> Cave
rockProcess cave rp wdrs = 
  case blowProcess cave rp (getNext wdrs) of
    FallingRock rp' -> 
      case dropProcess cave rp' of 
        FallingRock rp'' -> rockProcess cave rp'' (getRest wdrs)
        SettledRock cave' -> cave'


-- blow :: Cave -> WindDirection -> RockPosition -> Maybe RockPosition
-- blow = undefined

-- fall :: Cave -> RockPosition -> Maybe RockPosition
-- fall = undefined

-- bottomSurface :: Rock -> RockBottom
-- bottomSurface = undefined

-- canFall :: Cave -> RockBottom -> Bool
-- canFall = undefined

-- solidifyRock :: Cave -> Rock -> Cave
-- solidifyRock = undefined

-- startPosition :: Cave -> RockType -> Rock
-- startPosition = undefined

-- rockTypes :: [RockType]
-- rockTypes = cycle [HLine .. Square]

-- dropProcess :: Cave -> Rock -> 

--------------------------------------------------------------------------------

{-
  * The rocks always start with a blowing direction, not a drop
  * The blowing direction is stateful across rocks. 
  * The amount of drops is dependent on the cave. 
-}