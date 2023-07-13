module Problems.Day05 (
  part1Solution,
  part2Solution,
) where

import Data.Either (fromRight)
import Data.Functor (($>))
import Data.List (sortOn, uncons)
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (
  MonadParsec (try),
  Parsec,
  empty,
  optional,
  runParser,
  some,
  (<|>),
 )
import Text.Megaparsec.Char (char, letterChar, space1, spaceChar)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

data Slot = Empty | Crate Char deriving (Show, Eq)

data GridSlice = GridSlice
  { gridSliceSlot :: Slot
  , gridSliceXCoord :: Int
  }
  deriving (Show, Eq)

data GridPoint = GridPoint
  { gpSlot :: Slot
  , gpCoord :: (Int, Int)
  }
  deriving (Show, Eq)

type Move = (Int, Int, Int)

type Input = ([GridPoint], [Move])

type Depot = Map Int [Char]

--------------------------------------------------------------------------------

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
pLine =
  zipWith (curry buildSlice) [1 ..]
    <$> some (pSlot <* optional (char ' '))
 where
  buildSlice (xCoord, slot) = GridSlice slot xCoord

slicesToPoints :: Int -> [GridSlice] -> [GridPoint]
slicesToPoints yCoord = map $ \slice ->
  GridPoint (gridSliceSlot slice) (gridSliceXCoord slice, yCoord)

pStack :: Parser [GridPoint]
pStack = generatePoints . zip [1 ..] <$> some (pLine <* optional (char '\n'))
 where
  generatePoints = concatMap (uncurry slicesToPoints)

pLabels :: Parser [Int]
pLabels = spaceChar *> some integer

pInput :: Parser Input
pInput =
  (,)
    <$> (pStack <* pLabels)
    <*> some pMove

pMove :: Parser (Int, Int, Int)
pMove =
  (,,)
    <$> (symbol "move" *> integer)
    <*> (symbol "from" *> integer)
    <*> (symbol "to" *> integer)

--------------------------------------------------------------------------------

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

batchMoveCrates :: Move -> Depot -> Depot
batchMoveCrates (quantity, from, to) =
  uncurry (placeBatchOnStack to) . removeBatchFromStack quantity from

removeBatchFromStack :: Int -> Int -> Depot -> ([Char], Depot)
removeBatchFromStack quantity ident m =
  (batch, M.update (Just . const remaining) ident m)
 where
  (batch, remaining) = splitCrates m
  splitCrates = splitAt quantity . fromJust . M.lookup ident

placeBatchOnStack :: Int -> [Char] -> Depot -> Depot
placeBatchOnStack ident crates = M.update (Just . (++) crates) ident

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

--------------------------------------------------------------------------------

solution :: (Move -> Depot -> Depot) -> Text -> Text
solution mover input =
  topOfEachStack $ foldr mover depot $ reverse moves
 where
  structureInput = fromRight ([], []) . runParser pInput ""
  (points, moves) = structureInput input
  depot = buildDepot points

part1Solution :: Text -> Text
part1Solution = solution moveCrates

part2Solution :: Text -> Text
part2Solution = solution batchMoveCrates
