module Day11 where

import Data.Bifunctor (second)
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
type MonkeyProperties = (Item -> Item, Item -> Label)

newtype Item = Item Int deriving (Show, Eq)
newtype Label = Label Int deriving (Show, Eq, Ord)

type Circle = Map Label (MonkeyProperties, [Item])

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

applyOp :: MonkeyOp -> Int -> Int
applyOp (Add Nothing) n = 2 * n
applyOp (Add (Just k)) n = n + k
applyOp (Multiply Nothing) n = n * n
applyOp (Multiply (Just k)) n = n * k

pMonkey :: Parser (MonkeyProperties, [Item])
pMonkey = do
  items <- space *> pItems
  monkeyOp <- space *> pOperation
  divisor <- space *> pDivisibility
  throwLabels <- space *> pThrowChoice

  let modifier (Item n) = Item $ applyOp monkeyOp n `div` 3
      throwChoice item =
        case modifier item of
          Item n' ->
            if n' `mod` divisor == 0
              then fst throwLabels
              else snd throwLabels

  return ((modifier, throwChoice), items)

--------------------------------------------------------------------------------

determineTargets :: MonkeyProperties -> [Item] -> [(Label, Item)]
determineTargets (modifier, throwChoice) = 
  map $ \item -> (throwChoice item, modifier item)

sendItem :: Label -> Item -> Circle -> Circle
sendItem label item = M.adjust (second (++ [item])) label

sendAllItems :: Label -> [(Label, Item)] -> Circle -> Circle
sendAllItems thrower spread circle =
  M.adjust (second (const [])) thrower $
    foldr (uncurry sendItem) circle spread

-- turn :: (Label, (MonkeyProperties, [Item])) -> Circle -> Circle
-- turn (thrower, (mp, items)) circle =
--   case uncons items of

--     -- Removes all items from the thrower in the circle. 
--     Nothing -> M.adjust (second (const [])) thrower circle

--     -- Throws an item to another monkey in the circle. 
--     Just (item, rest) ->
--       let (modified, target) = (fst mp item, snd mp item)
--        in let circle' =
--                 M.adjust (second (++ [modified])) target circle
--            in turn (thrower, (mp, rest)) circle'

round :: Circle -> Circle
round circle = foldr sender circle $ M.assocs circle
  where
    sender (label, (mp, items)) inFlight = 
      let targets = determineTargets mp items
      in sendAllItems label targets inFlight