module D10Round2 where

newtype Register = Register Int deriving (Show, Eq, Ord)
newtype Cycle = Cycle Int deriving (Show, Eq, Ord)

data Simulation
  = Simulation
  { simRegister :: Register
  , simCycle    :: Cycle
  , simICycles  :: [ICycle]
  } deriving (Show, Eq)

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

tick :: ICycle -> Register -> (Register, Maybe ICycle)
tick (AddX v, 0) r = (incRegister v r, Nothing)
tick (AddX v, k) r = (r, Just (AddX v, k - 1))

tick (MulX v, 0) r = (mulRegister v r, Nothing)
tick (MulX v, k) r = (r, Just (MulX v, k - 1))

tick (DivX v, 0) r = (divRegister v r, Nothing)
tick (DivX v, k) r = (r, Just (DivX v, k - 1))

tick (Noop, _) r   = (r, Nothing)

addIns :: Instruction -> Simulation -> Simulation
addIns ins sim = sim { simICycles = make ins : simICycles sim }

iter :: Simulation -> Simulation
iter sim = simulateCPU sim
  where
    nextCycle = incCycle (simCycle sim)
    simulateCPU = buildSim nextCycle . foldr process (simRegister sim, []) . simICycles

buildSim :: Cycle -> (Register, [ICycle]) -> Simulation
buildSim cy (r, ics) = Simulation r cy ics

process :: ICycle -> (Register, [ICycle]) -> (Register, [ICycle])
process ic (r, ics) =
  case tick ic r of
    (r', Nothing) -> (r', ics)
    (r', Just op) -> (r', op:ics)

runCPU :: Int -> [Instruction] -> Simulation -> Simulation
runCPU 0 _ sim = sim
runCPU overflow [] sim = runCPU (overflow - 1) [] (iter sim)
runCPU overflow (m:ms) sim = runCPU overflow ms updated
  where
    updated = iter (addIns m sim)