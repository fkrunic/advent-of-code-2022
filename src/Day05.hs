{-# LANGUAGE OverloadedStrings #-}

module Day05 where

import Data.Either (fromRight)
import Data.Functor (($>))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Slot = Empty | Crate Char deriving (Show, Eq)

type Line = [(Int, Slot)]

type Move = (Int, Int, Int)

type Input = ([Line], [Move])

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

pLine :: Parser Line
pLine = zip [1 ..] <$> some (pSlot <* optional (char ' '))

pStack :: Parser [Line]
pStack = some (pLine <* optional (char '\n'))

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
