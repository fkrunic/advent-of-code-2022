{-# LANGUAGE OverloadedStrings #-}

module Day09 where

import           Data.Either                (fromRight)
import           Data.Functor               (($>))
import           Data.List                  (sortOn, uncons, scanr)
import           Data.Map                   (Map)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromJust)
import qualified Data.Set                   as S
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

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

data Instruction
  = AddX Int
  | Noop
  deriving (Show, Eq)

data Action
  = NoAction
  | IncrementRegistry Int
  deriving (Show, Eq)

newtype Registry = Registry Int deriving (Show, Eq)

newtype CycleCount = CycleCount Int deriving (Show, Eq)

newtype SignalStrength = SignalStrength Int deriving (Show, Eq)

data Snapshot
  = Snapshot
  { spEndRegistry :: Registry
  , spCycleCount :: CycleCount
  , spSignalStrength :: SignalStrength
  } deriving (Show, Eq)

------------------------------------------------------------------------------------

pInstruction :: Parser Instruction
pInstruction =
  choice
    [ symbol "noop" $> Noop,
      AddX <$> (symbol "addx" *> signedInteger)
    ]

------------------------------------------------------------------------------------

inc :: CycleCount -> CycleCount
inc (CycleCount c) = CycleCount (c + 1)

convertToAction :: Instruction -> [Action]
convertToAction Noop     = [NoAction]
convertToAction (AddX v) = [NoAction, IncrementRegistry v]

getActions :: [Instruction] -> [Action]
getActions = concatMap convertToAction

updateRegistry :: Action -> Registry -> Registry
updateRegistry NoAction r                         = r
updateRegistry (IncrementRegistry v) (Registry r) = Registry (r + v)

signalStrength :: Registry -> CycleCount -> SignalStrength
signalStrength (Registry r) (CycleCount c) = SignalStrength (r * c)

updateSnapshot :: Action -> Snapshot -> Snapshot
updateSnapshot action sp 
  = Snapshot
  { spEndRegistry = updateRegistry action (spEndRegistry sp)
  , spCycleCount = nextCycle
  , spSignalStrength = signalStrength (spEndRegistry sp) nextCycle
  }
  where
    nextCycle = inc (spCycleCount sp)

initialSp :: Snapshot
initialSp = Snapshot (Registry 1) (CycleCount 1) (SignalStrength 1)

runCPU :: Snapshot -> [Action] -> [Snapshot]
runCPU initial = reverse . scanr updateSnapshot initial . reverse

targetCycles :: [CycleCount]
targetCycles = map CycleCount [20, 60, 100, 140, 180, 220]

isTarget :: Snapshot -> Bool
isTarget sp = spCycleCount sp `elem` targetCycles

sumTargetSignals :: [Instruction] -> Int
sumTargetSignals = 
  sum . map (unpack . spSignalStrength) . filter isTarget . runCPU initialSp . getActions
  where
    unpack (SignalStrength s) = s

------------------------------------------------------------------------------------

part1Solution :: Text -> Int 
part1Solution = sumTargetSignals . fromRight [] . runParser (some pInstruction) ""

smallInput :: Text
smallInput =
  T.intercalate
    "\n"
    [ "noop",
      "addx 3",
      "addx -5"
    ]

puzzleInput :: Text
puzzleInput =
  T.intercalate
    "\n"
    [ "addx 15",
      "addx -11",
      "addx 6",
      "addx -3",
      "addx 5",
      "addx -1",
      "addx -8",
      "addx 13",
      "addx 4",
      "noop",
      "addx -1",
      "addx 5",
      "addx -1",
      "addx 5",
      "addx -1",
      "addx 5",
      "addx -1",
      "addx 5",
      "addx -1",
      "addx -35",
      "addx 1",
      "addx 24",
      "addx -19",
      "addx 1",
      "addx 16",
      "addx -11",
      "noop",
      "noop",
      "addx 21",
      "addx -15",
      "noop",
      "noop",
      "addx -3",
      "addx 9",
      "addx 1",
      "addx -3",
      "addx 8",
      "addx 1",
      "addx 5",
      "noop",
      "noop",
      "noop",
      "noop",
      "noop",
      "addx -36",
      "noop",
      "addx 1",
      "addx 7",
      "noop",
      "noop",
      "noop",
      "addx 2",
      "addx 6",
      "noop",
      "noop",
      "noop",
      "noop",
      "noop",
      "addx 1",
      "noop",
      "noop",
      "addx 7",
      "addx 1",
      "noop",
      "addx -13",
      "addx 13",
      "addx 7",
      "noop",
      "addx 1",
      "addx -33",
      "noop",
      "noop",
      "noop",
      "addx 2",
      "noop",
      "noop",
      "noop",
      "addx 8",
      "noop",
      "addx -1",
      "addx 2",
      "addx 1",
      "noop",
      "addx 17",
      "addx -9",
      "addx 1",
      "addx 1",
      "addx -3",
      "addx 11",
      "noop",
      "noop",
      "addx 1",
      "noop",
      "addx 1",
      "noop",
      "noop",
      "addx -13",
      "addx -19",
      "addx 1",
      "addx 3",
      "addx 26",
      "addx -30",
      "addx 12",
      "addx -1",
      "addx 3",
      "addx 1",
      "noop",
      "noop",
      "noop",
      "addx -9",
      "addx 18",
      "addx 1",
      "addx 2",
      "noop",
      "noop",
      "addx 9",
      "noop",
      "noop",
      "noop",
      "addx -1",
      "addx 2",
      "addx -37",
      "addx 1",
      "addx 3",
      "noop",
      "addx 15",
      "addx -21",
      "addx 22",
      "addx -6",
      "addx 1",
      "noop",
      "addx 2",
      "addx 1",
      "noop",
      "addx -10",
      "noop",
      "noop",
      "addx 20",
      "addx 1",
      "addx 2",
      "addx 2",
      "addx -6",
      "addx -11",
      "noop",
      "noop",
      "noop"
    ]
