{-# LANGUAGE OverloadedStrings #-}

module Day05 where

import Data.Either (fromRight)
import Data.Functor (($>))
import Data.List (sortOn, uncons)
import Data.Map (Map)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Slot = Empty | Crate Char deriving (Show, Eq)

data GridSlice
  = GridSlice
  { gridSliceSlot :: Slot
  , gridSliceXCoord :: Int
  } deriving (Show, Eq)

data GridPoint
  = GridPoint
  { gpSlot :: Slot
  , gpCoord :: (Int, Int)
  } deriving (Show, Eq)

type Line = [GridSlice]

type Move = (Int, Int, Int)

type Input = ([GridPoint], [Move])

type Depot = Map Int [Char]

------------------------------------------------------------------------------------

sc :: Parser ()
sc = L.space space1 empty empty

lexer :: Parser a -> Parser a
lexer = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Int
integer = lexer L.decimal

pSlot :: Parser Slot
pSlot = try pEmpty <|> pCrate
  where
    pEmpty = spaceChar *> spaceChar *> spaceChar $> Empty
    pCrate = Crate <$> (char '[' *> letterChar <* char ']')

pLine :: Parser [GridSlice]
pLine = map buildSlice . zip [1 ..] <$> some (pSlot <* optional (char ' '))
  where
    buildSlice (xCoord, slot) = GridSlice slot xCoord

slicesToPoints :: Int -> [GridSlice] -> [GridPoint]
slicesToPoints yCoord = map $ \slice -> 
  GridPoint (gridSliceSlot slice) (gridSliceXCoord slice, yCoord) 

pStack :: Parser [GridPoint]
pStack = generatePoints . zip [1..] <$> some (pLine <* optional (char '\n'))
  where
    generatePoints = concatMap (uncurry slicesToPoints)

pLabels :: Parser [Int]
pLabels = spaceChar *> some integer

pInput :: Parser Input
pInput =
  (,)
    <$> (pStack <* pLabels)
    <*> (some pMove)

pMove :: Parser (Int, Int, Int)
pMove =
  (,,)
    <$> (symbol "move" *> integer)
    <*> (symbol "from" *> integer)
    <*> (symbol "to" *> integer)

------------------------------------------------------------------------------------

addToStack :: Int -> Slot -> Depot -> Depot
addToStack _ Empty = id
addToStack ident (Crate c) = M.insertWith (++) ident [c]

buildDepot :: [GridPoint] -> Depot
buildDepot = foldr builder M.empty . sortOn (snd . gpCoord)
  where
    builder gp = addToStack (getX gp) (gpSlot gp)
    getX = fst . gpCoord    

moveCrates :: Move -> Depot -> Depot
moveCrates (quantity, from, to) m
  | quantity < 1 = m
  | otherwise = moveCrates (quantity - 1, from, to) (moveSingleCrate from to m)

moveSingleCrate :: Int -> Int -> Depot -> Depot
moveSingleCrate from to = uncurry (placeOnStack to) . removeFromStack from

removeFromStack :: Int -> Depot -> (Char, Depot)
removeFromStack ident m = fromJust payload
  where
    payload = do 
      crates <- M.lookup ident m
      (top, rest) <- uncons crates
      return (top, M.update (Just . const rest) ident m)

placeOnStack :: Int -> Char -> Depot -> Depot
placeOnStack ident c = M.update (Just . (:) c) ident

topOfEachStack :: Depot -> Text
topOfEachStack = T.pack . map (head . snd) . M.toList

------------------------------------------------------------------------------------

part1Solution :: Text -> Text
part1Solution input = topOfEachStack $ foldr moveCrates depot $ reverse moves
  where
    structureInput = fromRight ([], []) . runParser pInput ""
    (points, moves) = structureInput input
    depot = buildDepot points

------------------------------------------------------------------------------------

puzzleInput :: Text
puzzleInput =
  T.intercalate
    "\n"
    [ "    [D]    ",
      "[N] [C]    ",
      "[Z] [M] [P]",
      " 1   2   3 ",
      "",
      "move 1 from 2 to 1",
      "move 3 from 1 to 3",
      "move 2 from 2 to 1",
      "move 1 from 1 to 2"
    ]

example :: Text -> [GridPoint]
example = fst . fromRight ([], []) . runParser pInput ""