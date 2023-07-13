module Problems.Day09 (
  part1Solution,
  part2Solution,
) where

import Data.Either (fromRight)
import Data.List (nub)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, empty, runParser, some)
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L

newtype XCoordinate = XCoordinate Int deriving (Show, Eq)
newtype YCoordinate = YCoordinate Int deriving (Show, Eq)
newtype DeltaX = DeltaX Int deriving (Show, Eq)
newtype DeltaY = DeltaY Int deriving (Show, Eq)

type Coordinate = (XCoordinate, YCoordinate)
type Delta = (DeltaX, DeltaY)

newtype Knot = Knot Coordinate deriving (Show, Eq)
type Rope = [Knot]

data Move = UpMove | DownMove | LeftMove | RightMove deriving (Show, Eq)
data Instruction = Instruction Move Int deriving (Show, Eq)

type Parser = Parsec Void Text

--------------------------------------------------------------------------------

moveToShift :: Move -> Delta
moveToShift UpMove = (DeltaX 0, DeltaY (-1))
moveToShift DownMove = (DeltaX 0, DeltaY 1)
moveToShift LeftMove = (DeltaX (-1), DeltaY 0)
moveToShift RightMove = (DeltaX 1, DeltaY 0)

adjustFollowingKnot :: Knot -> Knot -> Knot
adjustFollowingKnot (Knot leading) (Knot following) = Knot shiftedFollow
 where
  currentDelta = findDelta leading following
  necessaryAdjustment = findAdjDelta currentDelta
  shiftedFollow = shiftCoordinate following necessaryAdjustment

adjustRope :: Rope -> Rope
adjustRope [] = []
adjustRope [knot] = [knot]
adjustRope (k1 : k2 : rest) = k1 : adjustRope (adjusted : rest)
 where
  adjusted = adjustFollowingKnot k1 k2

moveRope :: Rope -> Move -> Rope
moveRope [] _ = []
moveRope ((Knot headC) : rest) m = adjustRope (shiftedHead : rest)
 where
  shift = moveToShift m
  shiftedHead = Knot $ shiftCoordinate headC shift

shiftCoordinate :: Coordinate -> Delta -> Coordinate
shiftCoordinate (XCoordinate x, YCoordinate y) (DeltaX dx, DeltaY dy) =
  (XCoordinate (x + dx), YCoordinate (y + dy))

findDelta :: Coordinate -> Coordinate -> Delta
findDelta (XCoordinate x1, YCoordinate y1) (XCoordinate x2, YCoordinate y2) =
  (DeltaX (x1 - x2), DeltaY (y1 - y2))

mhDistance :: Delta -> Int
mhDistance (DeltaX dx, DeltaY dy) = abs dx + abs dy

reduce :: Int -> Int
reduce n
  | n > 0 = n - 1
  | otherwise = n + 1

findAdjDelta :: Delta -> Delta
findAdjDelta d@(DeltaX dx, DeltaY dy)
  -- deltas like (1,0) or (0,0)
  | mhDistance d <= 1 = zero
  -- deltas like (1,1)
  | mhDistance d == 2 && (abs dx == 1 || abs dy == 1) = zero
  -- deltas like (2,0)
  | mhDistance d <= 3 && (abs dx == 2 || abs dy == 2) =
      if abs dx == 2
        then (DeltaX (reduce dx), DeltaY dy)
        else (DeltaX dx, DeltaY (reduce dy))
  -- deltas like (2,2) that result from diagonal stretches
  | otherwise = (DeltaX (reduce dx), DeltaY (reduce dy))
 where
  zero = (DeltaX 0, DeltaY 0)

--------------------------------------------------------------------------------

sc :: Parser ()
sc = L.space space1 empty empty

lexer :: Parser a -> Parser a
lexer = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Int
integer = lexer L.decimal

pInstruction :: Parser Instruction
pInstruction =
  choice
    [ Instruction UpMove <$> (symbol "U" *> integer)
    , Instruction DownMove <$> (symbol "D" *> integer)
    , Instruction RightMove <$> (symbol "R" *> integer)
    , Instruction LeftMove <$> (symbol "L" *> integer)
    ]

instrToMoves :: Instruction -> [Move]
instrToMoves (Instruction m n) = replicate n m

--------------------------------------------------------------------------------

solution :: Int -> Text -> Int
solution nKnots =
  length
    . nub
    . getTailCs
    . movement
    . concatMap instrToMoves
    . parse
 where
  parse = fromRight [] . runParser (some pInstruction) ""
  rope = replicate nKnots (Knot (XCoordinate 1, YCoordinate 1))
  movement = reverse . scanr (flip moveRope) rope . reverse
  getCoordinate (Knot c) = c
  getTailCs = map (getCoordinate . last)

part1Solution :: Text -> Int
part1Solution = solution 2

part2Solution :: Text -> Int
part2Solution = solution 10
