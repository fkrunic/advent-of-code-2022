module Day11 where

import           Data.Either                (fromRight)
import           Data.Functor               (($>))
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Data.Void
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text
data MonkeyOp = Add (Maybe Int) | Multiply (Maybe Int) deriving (Show, Eq)
type ThrowChoice = (Int, Int)
data Monkey
  = Monkey 
  { label :: Int
  , startingItems :: [Int]
  , operation :: MonkeyOp
  , divisibility :: Int
  , throwChoice :: (Int, Int)
  }
  deriving (Show, Eq)

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

pMonkeyHeader :: Parser Int
pMonkeyHeader = symbol "Monkey" *> integer <* symbol ":"

pStartingItems :: Parser [Int]
pStartingItems = 
  symbol "Starting items:" *>
  some (integer <* optional (symbol ","))

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

pThrowChoice :: Parser (Int, Int)
pThrowChoice =
  (,) <$>
  (symbol "If true: throw to monkey" *> integer) <*>
  (symbol "If false: throw to monkey" *> integer)

pMonkey :: Parser Monkey
pMonkey = 
  Monkey <$> 
    pMonkeyHeader <*>
    (space *> pStartingItems) <*>
    (space *> pOperation) <*>
    (space *> pDivisibility) <*>
    (space *> pThrowChoice)

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
