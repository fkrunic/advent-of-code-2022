module Problems.Day13 (
  Comparison (..),
  organize,
  pInt,
  pList,
  pPair,
  valid,
) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, empty, many, optional)
import Text.Megaparsec.Char (newline, space1)
import Text.Megaparsec.Char.Lexer qualified as L

data Comparison
  = CInt Int
  | CList [Comparison]
  deriving (Show, Eq)

type Parser = Parsec Void Text

--------------------------------------------------------------------------------

sc :: Parser ()
sc = L.space space1 empty empty

lexer :: Parser a -> Parser a
lexer = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Int
integer = lexer L.decimal

pInt :: Parser Comparison
pInt = CInt <$> integer

pList :: Parser Comparison
pList =
  symbol "["
    *> (CList <$> many (pComparison <* optional (symbol ",")))
    <* symbol "]"

pComparison :: Parser Comparison
pComparison = choice [pInt, pList]

pPair :: Parser (Comparison, Comparison)
pPair =
  (,)
    <$> (pComparison <* optional newline)
    <*> (pComparison <* optional newline)

--------------------------------------------------------------------------------

valid :: Comparison -> Comparison -> Bool
valid (CInt i) (CInt j) = i <= j
valid (CList []) (CList _) = True
valid (CList _) (CList []) = False
valid (CList (x : xs)) (CList (y : ys))
  | x == y = valid (CList xs) (CList ys)
  | otherwise = valid x y
valid lone@(CInt _) xs = valid (CList [lone]) xs
valid xs lone@(CInt _) = valid xs (CList [lone])

organize :: Comparison -> Comparison -> Ordering
organize c1 c2 = if valid c1 c2 then LT else GT