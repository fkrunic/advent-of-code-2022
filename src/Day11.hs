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

data MonkeyOp = Add Int | Multiply Int deriving (Show, Eq)
type ThrowChoice = (Int, Int)
data Monkey
  = Monkey 
  { label :: Int
  , startingItems :: [Int]
  , operation :: MonkeyOp
  , throwChoice :: (Int, Int)
  }

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
pMonkeyHeader = symbol "Monkey" *> integer

pStartingItems :: Parser [Int]
pStartingItems = some (integer <* optional (symbol ","))

pMonkeyOp :: Parser MonkeyOp
pMonkeyOp = choice
  [ Add <$> (symbol "+" *> integer)
  , Multiply <$> (symbol "*" *> integer)
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


-------------------------------------------------------------------------------------

