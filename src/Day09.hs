module Day09 where

data HeadRelativeToTail
  = Covering
  | North
  | South
  | East
  | West
  | NorthWest
  | NorthEast
  | SouthWest
  | SouthEast
  deriving (Show, Eq, Enum)

data Move = UpMove | DownMove | LeftMove | RightMove deriving (Show, Eq)

newtype XCoordinate = XCoordinate Int deriving (Show, Eq, Ord)
newtype YCoordinate = YCoordinate Int deriving (Show, Eq, Ord)
type Coordinate = (XCoordinate, YCoordinate)
newtype HeadPosition = HeadPosition Coordinate deriving (Show, Eq)
newtype TailPosition = TailPosition Coordinate deriving (Show, Eq)
type Rope = (HeadPosition, HeadRelativeToTail)

dragTail :: HeadRelativeToTail -> Move -> HeadRelativeToTail
dragTail Covering UpMove     = North
dragTail North UpMove        = North
dragTail South UpMove        = Covering
dragTail East UpMove         = NorthEast
dragTail West UpMove         = NorthWest
dragTail NorthWest UpMove    = North
dragTail NorthEast UpMove    = North
dragTail SouthWest UpMove    = West
dragTail SouthEast UpMove    = East
dragTail Covering DownMove   = South
dragTail North DownMove      = Covering
dragTail South DownMove      = South
dragTail East DownMove       = SouthEast
dragTail West DownMove       = SouthWest
dragTail NorthWest DownMove  = West
dragTail NorthEast DownMove  = East
dragTail SouthWest DownMove  = South
dragTail SouthEast DownMove  = South
dragTail Covering LeftMove   = West
dragTail North LeftMove      = NorthWest
dragTail South LeftMove      = NorthWest
dragTail East LeftMove       = Covering
dragTail West LeftMove       = West
dragTail NorthWest LeftMove  = West
dragTail NorthEast LeftMove  = North
dragTail SouthWest LeftMove  = West
dragTail SouthEast LeftMove  = South
dragTail Covering RightMove  = East
dragTail North RightMove     = NorthEast
dragTail South RightMove     = SouthEast
dragTail East RightMove      = East
dragTail West RightMove      = Covering
dragTail NorthWest RightMove = North
dragTail NorthEast RightMove = East
dragTail SouthWest RightMove = South
dragTail SouthEast RightMove = East

moveHead :: HeadPosition -> Move -> HeadPosition
moveHead (HeadPosition (xCoord, YCoordinate y)) UpMove = HeadPosition (xCoord, YCoordinate (y - 1))
moveHead (HeadPosition (xCoord, YCoordinate y)) DownMove = HeadPosition (xCoord, YCoordinate (y + 1))
moveHead (HeadPosition (XCoordinate x, yCoord)) LeftMove = HeadPosition (XCoordinate (x - 1), yCoord)
moveHead (HeadPosition (XCoordinate x, yCoord)) RightMove = HeadPosition (XCoordinate (x + 1), yCoord)

deduceTailPosition :: HeadPosition -> HeadRelativeToTail -> TailPosition
deduceTailPosition (HeadPosition c) Covering = TailPosition c
deduceTailPosition (HeadPosition (xCoord, YCoordinate y)) North = TailPosition (xCoord, YCoordinate (y + 1))
deduceTailPosition (HeadPosition (xCoord, YCoordinate y)) South = TailPosition (xCoord, YCoordinate (y - 1))
deduceTailPosition (HeadPosition (XCoordinate x, yCoord)) East = TailPosition (XCoordinate (x - 1), yCoord)
deduceTailPosition (HeadPosition (XCoordinate x, yCoord)) West = TailPosition (XCoordinate (x + 1), yCoord)
deduceTailPosition (HeadPosition (XCoordinate x, YCoordinate y)) NorthWest = TailPosition (XCoordinate (x + 1), YCoordinate (y + 1))
deduceTailPosition (HeadPosition (XCoordinate x, YCoordinate y)) NorthEast = TailPosition (XCoordinate (x - 1), YCoordinate (y + 1))
deduceTailPosition (HeadPosition (XCoordinate x, YCoordinate y)) SouthWest = TailPosition (XCoordinate (x + 1), YCoordinate (y - 1))
deduceTailPosition (HeadPosition (XCoordinate x, YCoordinate y)) SouthEast = TailPosition (XCoordinate (x - 1), YCoordinate (y - 1))

moveRope :: Rope -> Move -> Rope
moveRope (hp, hrp) m = (moveHead hp m, dragTail hrp m)

