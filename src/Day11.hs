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

type Parser = Parsec Void Text
data MonkeyOp = Add (Maybe Int) | Multiply (Maybe Int) deriving (Show, Eq)
type Bananza = Map Label Monkey

data Monkey = Monkey
  { items :: [Item]
  , operation :: MonkeyOp
  , divisibility :: Int
  , throwChoice :: (Label, Label)
  }
  deriving (Show, Eq)

newtype Item = Item Int deriving (Show, Eq)
newtype Label = Label Int deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------

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

pEntry :: Parser (Label, Monkey)
pEntry =
  (,)
    <$> pLabel
    <*> pMonkey

--------------------------------------------------------------------------------

applyOp :: Int -> MonkeyOp -> Int
applyOp n (Add Nothing) = 2 * n
applyOp n (Add (Just k)) = n + k
applyOp n (Multiply Nothing) = n * n
applyOp n (Multiply (Just k)) = n * k

relieveWorry :: Int -> Int
relieveWorry n = n `div` 3

throw :: Label -> (Item, [Item]) -> Monkey -> Bananza -> Bananza
throw label (Item worry, remainingItems) m ms =
  M.update
    (\thrower -> Just $ thrower{items = remainingItems})
    label
    addItemToTarget
 where
  relieved = relieveWorry (applyOp worry (operation m))
  targetMonkey =
    if relieved `mod` divisibility m == 0
      then fst (throwChoice m)
      else snd (throwChoice m)
  addItemToTarget =
    M.update
      ( \target ->
          Just $ target{items = items target ++ [Item relieved]}
      )
      targetMonkey
      ms

inspect :: Label -> Bananza -> Bananza
inspect label ms =
  case uncons (items m) of
    Nothing -> ms
    Just unpacked -> throw label unpacked m ms
 where
  m = (M.!) ms label

-- throw :: Monkey -> Item -> [(Item, Label)] -> [(Item, Label)]
-- throw m (Item worry) labels =
--   if relieved `mod` divisibility m == 0
--     then (Item relieved, fst (throwChoice m)) : labels
--     else (Item relieved, snd (throwChoice m)) : labels
--  where
--   worried = modifyWorry worry (operation m)
--   relieved = relief worried

-- scatter :: Monkey -> [(Item, Label)]
-- scatter m = foldr (throw m) [] . startingItems $ m

-- singleMonkeyTurn :: Label -> [(Item, Label)] -> Bananza -> Bananza
-- singleMonkeyTurn thrower flyers bnz = sendFlyers
--  where
--   liquidateThrower = M.update (Just . emptyStartingItems) thrower bnz
--   sendFlyers = foldr send liquidateThrower flyers
--   send (item, toMonkey) = M.update (Just . throwAt item) toMonkey
--   emptyStartingItems mk = mk{startingItems = []}

-- throwAt :: Item -> Monkey -> Monkey
-- throwAt item m = m{startingItems = startingItems m ++ [item]}

-- turn :: Bananza -> Bananza
-- turn bnz = foldr executeTurn bnz $ reverse $ M.elems bnz
--  where
--   executeTurn m = singleMonkeyTurn (label m) (scatter m)

--------------------------------------------------------------------------------
