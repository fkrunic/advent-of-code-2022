module Problems.Day11 where

import Control.Monad (forM_, replicateM_)
import Control.Monad.Trans.State.Strict (State, execState, get, modify)
import Data.Bifunctor (first, second)
import Data.Functor (($>))
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, empty, optional, some)
import Text.Megaparsec.Char (space, space1)
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (round)

type Parser = Parsec Void Text

data MonkeyOp = Add (Maybe Int) | Multiply (Maybe Int) deriving (Show, Eq)

newtype Item = Item Int deriving (Show, Eq)
newtype Label = Label Int deriving (Show, Eq, Ord)
newtype Reducer = Reducer Int deriving (Show, Eq, Ord)
newtype Times = Times Int deriving (Show, Eq, Ord)

data Monkey = Monkey
  { label :: Label
  , items :: [Item]
  , operation :: MonkeyOp
  , divisor :: Int
  , throwChoices :: (Label, Label)
  }
  deriving (Show, Eq)

data MonkeyState = MonkeyState
  { counter :: Int
  , holding :: [Item]
  }
  deriving (Show, Eq)

type MonkeyItems = State (Map Label MonkeyState)

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
    <$> pLabel
    <*> (space *> pItems)
    <*> (space *> pOperation)
    <*> (space *> pDivisibility)
    <*> (space *> pThrowChoice)

--------------------------------------------------------------------------------

applyOp :: MonkeyOp -> Int -> Int
applyOp (Add Nothing) n = 2 * n
applyOp (Add (Just k)) n = n + k
applyOp (Multiply Nothing) n = n * n
applyOp (Multiply (Just k)) n = n * k

addItem :: Item -> MonkeyState -> MonkeyState
addItem item ms = ms{holding = holding ms ++ [item]}

reset :: MonkeyState -> MonkeyState
reset ms = ms{holding = []}

inc :: MonkeyState -> MonkeyState
inc ms = ms{counter = strictAdd (counter ms) 1}

strictAdd :: Int -> Int -> Int
strictAdd = ($!) (+)

round ::
  Int ->
  [Label] ->
  Map Label Monkey ->
  MonkeyItems ()
round reducer mkLabels mkProperties =
  forM_ mkLabels $ \label -> do
    items <- holding . (! label) <$> get
    forM_ items $ \(Item worry) -> do
      let props = mkProperties ! label
          modified = applyOp (operation props) worry `div` reducer
          chooser = if modified `mod` divisor props == 0 then fst else snd
          throwTarget = chooser (throwChoices props)
      modify $ M.adjust (addItem (Item modified)) throwTarget
      modify $ M.adjust inc label
    modify $ M.adjust reset label

getItems :: [Monkey] -> Map Label MonkeyState
getItems = M.fromList . map (\m -> (label m, MonkeyState 0 (items m)))

runRounds ::
  Reducer ->
  Times ->
  [Label] ->
  Map Label Monkey ->
  Map Label MonkeyState ->
  Map Label MonkeyState
runRounds (Reducer r) (Times t) mkLabels mkProperties =
  execState $ replicateM_ t (round r mkLabels mkProperties)

--------------------------------------------------------------------------------

newtype Factor = Factor Int deriving (Show, Eq, Ord, Num)
newtype Residue = Residue Int deriving (Show, Eq, Ord, Num)
newtype Counter = Counter Int deriving (Show, Eq, Ord, Num)

newtype Index = Index Int deriving (Show, Eq, Ord)
newtype Worry = Worry Int deriving (Show, Eq, Ord)

type Modifier = Int -> Int
type IndexedItem = (Label, Index, Worry)

type ResidueMap = Map Factor Residue
type ItemResiduals = Map Index ResidueMap
type MonkeyCounters = Map Label Counter

data MonkeyIndexedState = MonkeyIndexedState
  { residueCounter :: Counter
  , indexedHolding :: [IndexedItem]
  }
  deriving (Show, Eq)

type CountingGame = State (Map Label MonkeyIndexedState, ItemResiduals)

applyModifier :: Modifier -> ResidueMap -> ResidueMap
applyModifier modifier = M.mapWithKey updater
 where
  updater (Factor f) (Residue r) = Residue $ modifier r `mod` f

applyModifierToIndex :: Index -> Modifier -> ItemResiduals -> ItemResiduals
applyModifierToIndex index modifier = M.adjust (applyModifier modifier) index

getFactor :: Monkey -> Factor
getFactor = Factor . divisor

getFactors :: [Monkey] -> [Factor]
getFactors = map getFactor

generateIndexedItems :: [Monkey] -> [IndexedItem]
generateIndexedItems =
  zipWith binder [0 ..]
    . concatMap (\m -> map (label m,) $ items m)
 where
  binder index (label, Item worry) = (label, Index index, Worry worry)

buildResidueMap :: IndexedItem -> [Factor] -> ResidueMap
buildResidueMap (_, _, Worry worry) = M.fromList . map (normalizer worry)
 where
  normalizer w (Factor f) = (Factor f, Residue $ w `mod` f)

buildResiduals :: [IndexedItem] -> [Factor] -> ItemResiduals
buildResiduals items factors =
  M.fromList $ map builder items
 where
  builder item@(_, Index index, _) = (Index index, buildResidueMap item factors)

residueRound ::
  [Label] ->
  Map Label Monkey ->
  CountingGame ()
residueRound mkLabels mkProperties =
  forM_ mkLabels $ \label -> do
    items <- indexedHolding . (! label) . fst <$> get
    forM_ items $ \indexedItem@(_, index, _) -> do
      let props = mkProperties ! label
          factor = Factor (divisor props)
          modifier = applyOp (operation props)
      modify $ second (applyModifierToIndex index modifier)
      residueWorry <- (! factor) . (! index) . snd <$> get
      let chooser = if residueWorry == Residue 0 then fst else snd
          throwTarget = chooser (throwChoices props)
      modify $ first $ M.adjust (addItem' indexedItem) throwTarget
      modify $ first $ M.adjust inc' label
    modify $ first $ M.adjust reset' label

addItem' :: IndexedItem -> MonkeyIndexedState -> MonkeyIndexedState
addItem' item mis = mis{indexedHolding = indexedHolding mis ++ [item]}

reset' :: MonkeyIndexedState -> MonkeyIndexedState
reset' ms = ms{indexedHolding = []}

inc' :: MonkeyIndexedState -> MonkeyIndexedState
inc' mis = mis{residueCounter = residueCounter mis + Counter 1}

runResidues ::
  Times ->
  [Label] ->
  Map Label Monkey ->
  (Map Label MonkeyIndexedState, ItemResiduals) ->
  Map Label MonkeyIndexedState
runResidues (Times t) mkLabels mkProperties =
  fst . execState (replicateM_ t (residueRound mkLabels mkProperties))

initialIndexedState :: [Monkey] -> Map Label MonkeyIndexedState
initialIndexedState mks = M.fromList $ map builder mks
 where
  indexedItems = generateIndexedItems mks
  builder m =
    ( label m
    , MonkeyIndexedState (Counter 0) $
        filter (\(lbl, _, _) -> lbl == label m) indexedItems
    )