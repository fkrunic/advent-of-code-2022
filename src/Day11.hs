module Day11 where

import Data.Functor (($>))
import Data.List (uncons)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, empty, optional, some)
import Text.Megaparsec.Char (space, space1)
import Text.Megaparsec.Char.Lexer qualified as L

--------------------------------------------------------------------------------

data MonkeyOp = Add (Maybe Int) | Multiply (Maybe Int) deriving (Show, Eq)

data Monkey = Monkey
  { items :: [Item]
  , operation :: MonkeyOp
  , divisibility :: Int
  , throwChoice :: (Label, Label)
  }
  deriving (Show, Eq)

newtype Item = Item Int deriving (Show, Eq)
newtype Label = Label Int deriving (Show, Eq, Ord)

applyOp :: Int -> MonkeyOp -> Int
applyOp n (Add Nothing) = 2 * n
applyOp n (Add (Just k)) = n + k
applyOp n (Multiply Nothing) = n * n
applyOp n (Multiply (Just k)) = n * k

worryMod :: Monkey -> Item -> Item
worryMod monkey (Item worry) = 
  Item $ applyOp worry (operation monkey) `div` 3

deduceTarget :: Monkey -> Item -> Label 
deduceTarget monkey item = 
  case worryMod monkey item of 
    Item w' -> 
      if w' `mod` (divisibility monkey) == 0
        then fst (throwChoice monkey)
        else snd (throwChoice monkey)

-- relieveWorry :: Int -> Int
-- relieveWorry n = n `div` 3

-- throw :: Label -> (Item, [Item]) -> Monkey -> Bananza -> Bananza
-- throw label (Item worry, remainingItems) m ms =
--   M.update
--     (\thrower -> Just $ thrower{items = remainingItems})
--     label
--     addItemToTarget
--  where
--   relieved = relieveWorry (applyOp worry (operation m))
--   targetMonkey =
--     if relieved `mod` divisibility m == 0
--       then fst (throwChoice m)
--       else snd (throwChoice m)
--   addItemToTarget =
--     M.update
--       ( \target ->
--           Just $ target{items = items target ++ [Item relieved]}
--       )
--       targetMonkey
--       ms

-- inspect :: Label -> Bananza -> Bananza
-- inspect label ms =
--   case uncons (items m) of
--     Nothing -> ms
--     Just unpacked -> throw label unpacked m ms
--  where
--   m = (M.!) ms label


--------------------------------------------------------------------------------

type Parser = Parsec Void Text

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

pLabel :: Parser Label
pLabel =
  Label <$> (symbol "Monkey" *> integer <* symbol ":")

pItems :: Parser [Item]
pItems =
  symbol "Starting items:"
    *> some (Item <$> integer <* optional (symbol ","))

pOpReference :: Parser (Maybe Int)
pOpReference =
  choice
    [ symbol "old" $> Nothing
    , Just <$> integer
    ]

pMonkeyOp :: Parser MonkeyOp
pMonkeyOp =
  choice
    [ Add <$> (symbol "+" *> pOpReference)
    , Multiply <$> (symbol "*" *> pOpReference)
    ]

pOperation :: Parser MonkeyOp
pOperation = symbol "Operation: new = old" *> pMonkeyOp

pDivisibility :: Parser Int
pDivisibility = symbol "Test: divisible by" *> integer

pThrowChoice :: Parser (Label, Label)
pThrowChoice =
  (,)
    <$> (Label <$> (symbol "If true: throw to monkey" *> integer))
    <*> (Label <$> (symbol "If false: throw to monkey" *> integer))

pMonkey :: Parser Monkey
pMonkey =
  Monkey
    <$> (space *> pItems)
    <*> (space *> pOperation)
    <*> (space *> pDivisibility)
    <*> (space *> pThrowChoice)
