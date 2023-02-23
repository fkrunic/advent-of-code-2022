module Day17 where

import Data.List

import Grids

data RockType = HLine | Plus | LShape | VLine | Square
  deriving (Show, Eq, Ord, Enum)

data WindDirection = East | West deriving (Show, Eq, Ord)
data Action = Blow WindDirection | Fall deriving (Show, Eq, Ord)

newtype CaveBoundaries = CaveBoundaries Boundaries deriving (Show, Eq)
data Cell = Empty | Surface deriving (Show, Eq)
type Cave = Grid Cell

newtype RockPosition = RockPosition [Coordinate] deriving (Show, Eq)
newtype RockBottom = RockBottom [Coordinate] deriving (Show, Eq)
type Rock = (RockType, RockPosition)

newtype RocksDropped = RocksDropped Int deriving (Show, Eq)
newtype TowerHeight = TowerHeight Int deriving (Show, Eq)

data TowerProcess = TowerProcess
  { cave :: Cave
  , rocksDropped :: Int
  , towerHeight :: Int
  }
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------

blow :: CaveBoundaries -> WindDirection -> RockPosition -> Maybe RockPosition
blow = undefined

fall :: CaveBoundaries -> RockPosition -> Maybe RockPosition
fall = undefined

bottomSurface :: Rock -> RockBottom
bottomSurface = undefined

canFall :: Cave -> RockBottom -> Bool
canFall = undefined

comeToRest :: Cave -> Rock -> Cave
comeToRest = undefined

dropPosition :: Cave -> RockType -> Rock
dropPosition = undefined

rockTypes :: [RockType]
rockTypes = cycle [HLine .. Square]

--------------------------------------------------------------------------------

{-
  * The rocks always start with a blowing direction, not a drop
  * The blowing direction is stateful across rocks. 
  * The amount of drops is dependent on the cave. 
-}