module Day11 where

import           Data.Either                (fromRight)
import           Data.Functor               (($>))
import           Data.Map                   (Map)
import qualified Data.Map.Strict            as M
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Data.Void
import           Text.Megaparsec            hiding (Label, State, label)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text
data MonkeyOp = Add (Maybe Int) | Multiply (Maybe Int) deriving (Show, Eq)
type ThrowChoice = (Int, Int)
type Bananza = Map Label Monkey

data Monkey
  = Monkey
  { label         :: Label
  , startingItems :: [Item]
  , operation     :: MonkeyOp
  , divisibility  :: Int
  , throwChoice   :: (Label, Label)
  }
  deriving (Show, Eq)

newtype Item = Item Int deriving (Show, Eq)
newtype Label = Label Int deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------------

sc :: Parser ()
sc = L.space space1 empty empty

lexer :: Parser a -> Parser a
lexer = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Int
integer = lexer L.decimal

signedInteger :: Parser Int
signedInteger = L.signed sc integer

pMonkeyHeader :: Parser Label
pMonkeyHeader =
  Label <$> (symbol "Monkey" *> integer <* symbol ":")

pStartingItems :: Parser [Item]
pStartingItems =
  symbol "Starting items:" *>
  some (Item <$> integer <* optional (symbol ","))

pOpReference :: Parser (Maybe Int)
pOpReference =
  choice
  [ symbol "old" $> Nothing
  , Just <$> integer
  ]

pMonkeyOp :: Parser MonkeyOp
pMonkeyOp = choice
  [ Add <$> (symbol "+" *> pOpReference)
  , Multiply <$> (symbol "*" *> pOpReference)
  ]

pOperation :: Parser MonkeyOp
pOperation = symbol "Operation: new = old" *> pMonkeyOp

pDivisibility :: Parser Int
pDivisibility = symbol "Test: divisible by" *> integer

pThrowChoice :: Parser (Label, Label)
pThrowChoice =
  (,) <$>
  (Label <$> (symbol "If true: throw to monkey" *> integer)) <*>
  (Label <$> (symbol "If false: throw to monkey" *> integer))

pMonkey :: Parser Monkey
pMonkey =
  Monkey <$>
    pMonkeyHeader <*>
    (space *> pStartingItems) <*>
    (space *> pOperation) <*>
    (space *> pDivisibility) <*>
    (space *> pThrowChoice)

-------------------------------------------------------------------------------------

modifyWorry :: Int -> MonkeyOp -> Int
modifyWorry n (Add Nothing)       = 2 * n
modifyWorry n (Add (Just k))      = n + k
modifyWorry n (Multiply Nothing)  = n * n
modifyWorry n (Multiply (Just k)) = n * k

relief :: Int -> Int
relief n = n `div` 3

scatter :: Monkey -> [(Item, Label)]
scatter m = thrower m
  where
    thrower = foldr throw [] . startingItems
    throw (Item worry) labels =
      let worried = modifyWorry worry (operation m)
      in let relieved = relief worried
      in if relieved `mod` divisibility m == 0
        then (Item relieved, fst (throwChoice m)) : labels
        else (Item relieved, snd (throwChoice m)) : labels

singleMonkeyTurn :: Label -> [(Item, Label)] -> Bananza -> Bananza
singleMonkeyTurn thrower flyers bnz = sendFlyers
  where
    liquidateThrower = M.update (\mk -> Just mk { startingItems = [] }) thrower bnz
    sendFlyers = foldr send liquidateThrower flyers
    send (item, label) = M.update (Just . throwAt item) label

throwAt :: Item -> Monkey -> Monkey
throwAt item m = m { startingItems = startingItems m ++ [item] }

turn :: Bananza -> Bananza
turn bnz = foldr executeTurn bnz $ reverse $ M.elems bnz
  where
    executeTurn m = singleMonkeyTurn (label m) (scatter m)

-- throwAt :: (Item, Label) -> Bananza -> Bananza
-- throwAt (item, label) = M.update attach label
--   where
--     attach m = Just $ m { startingItems = startingItems m ++ [item]}

-------------------------------------------------------------------------------------

puzzleInput :: Text
puzzleInput =
  T.intercalate "\n"
  [ "Monkey 0:"
  , "  Starting items: 79, 98"
  , "  Operation: new = old * 19"
  , "  Test: divisible by 23"
  , "    If true: throw to monkey 2"
  , "    If false: throw to monkey 3"
  , ""
  , "Monkey 1:"
  , "  Starting items: 54, 65, 75, 74"
  , "  Operation: new = old + 6"
  , "  Test: divisible by 19"
  , "    If true: throw to monkey 2"
  , "    If false: throw to monkey 0"
  , ""
  , "Monkey 2:"
  , "  Starting items: 79, 60, 97"
  , "  Operation: new = old * old"
  , "  Test: divisible by 13"
  , "    If true: throw to monkey 1"
  , "    If false: throw to monkey 3"
  , ""
  , "Monkey 3:"
  , "  Starting items: 74"
  , "  Operation: new = old + 3"
  , "  Test: divisible by 17"
  , "    If true: throw to monkey 0"
  , "    If false: throw to monkey 1"
  ]
