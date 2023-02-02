{-# LANGUAGE OverloadedStrings #-}

module D10Round2 where

import Data.List (uncons)
import           Control.Monad               (when)
import           Control.Monad.Trans.RWS.CPS
import           Data.Functor                (($>))
import           Data.Text                   (Text)
import qualified Data.Text                   as T

import           Data.Void
import           Text.Megaparsec             hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer  as L

type Parser = Parsec Void Text

newtype Register = Register Int deriving (Show, Eq, Ord)
newtype Cycle = Cycle Int deriving (Show, Eq, Ord)
newtype SignalStrength = SignalStrength Int deriving (Show, Eq)
type State = (Register, Cycle, [ICycle], [Instruction])
type Simulation = RWS () [SignalStrength] State

data Instruction
  = AddX Int -- 2 cycles
  | MulX Int -- 3 cycles
  | DivX Int -- 2 cycles
  | Noop     -- 1 cycle
  deriving (Show, Eq)

type ICycle = (Instruction, Int)

make :: Instruction -> ICycle
make (AddX v) = (AddX v, 2)
make (MulX v) = (MulX v, 3)
make (DivX v) = (DivX v, 2)
make Noop     = (Noop, 1)

incRegister :: Int -> Register -> Register
incRegister v (Register r) = Register (v + r)

mulRegister :: Int -> Register -> Register
mulRegister v (Register r) = Register (v * r)

divRegister :: Int -> Register -> Register
divRegister v (Register r) = Register (r `div` v)

incCycle :: Cycle -> Cycle
incCycle (Cycle c) = Cycle (c + 1)

-- addToICS :: Instruction -> Simulation ()
-- addToICS ins = modify adder
--   where
--     adder (r, c, ics) = (r, c, make ins : ics)

runInstructionCycle :: ICycle -> Register -> (Register, Maybe ICycle)
runInstructionCycle (AddX v, 0) r = (incRegister v r, Nothing)
runInstructionCycle (AddX v, k) r = (r, Just (AddX v, k - 1))

runInstructionCycle (MulX v, 0) r = (mulRegister v r, Nothing)
runInstructionCycle (MulX v, k) r = (r, Just (MulX v, k - 1))

runInstructionCycle (DivX v, 0) r = (divRegister v r, Nothing)
runInstructionCycle (DivX v, k) r = (r, Just (DivX v, k - 1))

runInstructionCycle (Noop, _) r   = (r, Nothing)

process :: ICycle -> (Register, [ICycle]) -> (Register, [ICycle])
process ic (r, ics) =
  case runInstructionCycle ic r of
    (r', Nothing) -> (r', ics)
    (r', Just op) -> (r', op:ics)

signalStrength :: Register -> Cycle -> SignalStrength
signalStrength (Register r) (Cycle c) = SignalStrength (r * c)

targetCycles :: [Cycle]
targetCycles = map Cycle [20, 60, 100, 140, 180, 220]

-- cpuCycle :: Simulation ()
-- cpuCycle = do
--   (register, cc, pendingICS, ins) <- get

--   -- case uncons ins of 
--   --   Nothing -> 
--   --     if null pendingICS 
--   --       then return ()
--   --       else cpuCycle

--   --   Just (m, remainingMoves) -> 

--   -- Record signal strength during cycle, before register has been updated.
--   when (cc `elem` targetCycles) $ do
--       tell [signalStrength (Register 1) cc]

--   -- Process all commmands and compute the updated state.
--   let (updatedRegister, processedICS) = foldr process (register, []) pendingICS

--   -- Write the updated state with the incremented cycle.
--   put (updatedRegister, incCycle cc, processedICS)

-- runSimulation :: [Instruction] -> [SignalStrength]
-- runSimulation ins = extractSS $ runRWS (mapM_ cpuCycle ins) () initial
--   where
--     initial = (Register 1, Cycle 1, [])
--     extractSS (_, _, ss) = ss

-----------------------------------------------------------------------------------------------


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

pInstruction :: Parser Instruction
pInstruction =
  choice
    [ symbol "noop" $> Noop,
      AddX <$> (symbol "addx" *> signedInteger)
    ]

-----------------------------------------------------------------------------------------------

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


-- addIns :: Instruction -> Simulation -> Simulation
-- addIns ins sim = sim { simICycles = make ins : simICycles sim }

-- iter :: Simulation -> Simulation
-- iter sim = simulateCPU sim
--   where
--     nextCycle = incCycle (simCycle sim)
--     simulateCPU = buildSim nextCycle . foldr process (simRegister sim, []) . simICycles

-- buildSim :: Cycle -> (Register, [ICycle]) -> Simulation
-- buildSim cy (r, ics) = Simulation r cy ics



-- runCPU :: Int -> [Instruction] -> Simulation -> Simulation
-- runCPU 0 _ sim = sim
-- runCPU overflow [] sim = runCPU (overflow - 1) [] (iter sim)
-- runCPU overflow (m:ms) sim = runCPU overflow ms updated
--   where
--     updated = iter (addIns m sim)
