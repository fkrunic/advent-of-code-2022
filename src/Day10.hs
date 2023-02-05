module Day10 (
  part1Solution,
  part2Solution,
  puzzleInput,
) where

import Data.Either (fromRight)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Data.Void
import Text.Megaparsec hiding (State, parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

newtype Register = Register Int deriving (Show, Eq, Ord)
newtype Cycle = Cycle Int deriving (Show, Eq, Ord)
newtype SignalStrength = SignalStrength Int deriving (Show, Eq)
newtype CRTRow = CRTRow String deriving (Show, Eq)
newtype SpriteLine = SpriteLine String deriving (Show, Eq)

type Simulation =
  ( Action
  , Register
  , Cycle
  , Maybe SignalStrength
  , SpriteLine
  , CRTRow
  )

data Instruction
  = AddX Int -- 2 cycles
  | Noop -- 1 cycle
  deriving (Show, Eq)

data Action
  = BeginCycle
  | DuringCycle
  | EndCycle
  | EndCycleIncrementRegister Int
  deriving (Show, Eq)

type Parser = Parsec Void Text

--------------------------------------------------------------------------------

inc :: Cycle -> Cycle
inc (Cycle c) = Cycle (c + 1)

add :: Int -> Register -> Register
add v (Register r) = Register (v + r)

signal :: Register -> Cycle -> SignalStrength
signal (Register r) (Cycle c) = SignalStrength (r * c)

insToActions :: Instruction -> [Action]
insToActions Noop = [BeginCycle, DuringCycle, EndCycle]
insToActions (AddX v) =
  [ BeginCycle
  , DuringCycle
  , EndCycle
  , BeginCycle
  , DuringCycle
  , EndCycleIncrementRegister v
  ]

update :: Action -> Simulation -> Simulation
update BeginCycle (_, register, cyc, _, _, crt) =
  (BeginCycle, register, inc cyc, Nothing, spritePosition register, crt)
update DuringCycle (_, register, cyc, _, sl, crt) =
  ( DuringCycle
  , register
  , cyc
  , Just (signal register cyc)
  , sl
  , updateCRTRow cyc crt sl
  )
update EndCycle (_, register, cyc, _, sl, crt) =
  (EndCycle, register, cyc, Nothing, sl, crt)
update (EndCycleIncrementRegister v) (_, register, cyc, _, sl, crt) =
  (EndCycleIncrementRegister v, add v register, cyc, Nothing, sl, crt)

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

pInstruction :: Parser Instruction
pInstruction =
  choice
    [ symbol "noop" $> Noop
    , AddX <$> (symbol "addx" *> signedInteger)
    ]

--------------------------------------------------------------------------------

determineCRTLength :: Int -> Int
determineCRTLength i =
  if 1 <= i && i <= 40
    then i
    else determineCRTLength (i - 40)

spritePosition :: Register -> SpriteLine
spritePosition (Register r) =
  SpriteLine $ take 40 $ front ++ middle ++ back
 where
  startIndex = r - 1
  front = replicate startIndex '.'
  middle = "###"
  back = repeat '.'

findNextPixel :: CRTRow -> SpriteLine -> Char
findNextPixel (CRTRow row) (SpriteLine sl) =
  fromMaybe '.' $ lookup (length row) assoc
 where
  assoc = zip [0 ..] sl

updateCRTRow :: Cycle -> CRTRow -> SpriteLine -> CRTRow
updateCRTRow (Cycle c) crt@(CRTRow row) sl = updatedCRT
 where
  crtLength = determineCRTLength c
  updatedCRT = CRTRow $ take crtLength $ row ++ [findNextPixel crt sl]

renderingCycles :: [Cycle]
renderingCycles = map Cycle [40, 80, 120, 160, 200, 240]

--------------------------------------------------------------------------------

targetCycles :: [Cycle]
targetCycles = map Cycle [20, 60, 100, 140, 180, 220]

initialSimState :: Simulation
initialSimState =
  ( BeginCycle
  , Register 1
  , Cycle 0
  , Nothing
  , spritePosition (Register 1)
  , CRTRow ""
  )

part1Solution :: Text -> Int
part1Solution =
  sum
    . map extractSS
    . filter isTargetCycle
    . runner
    . concatMap insToActions
    . parse
 where
  parse = fromRight [] . runParser (some pInstruction) ""
  runner = reverse . scanr update initialSimState . reverse
  isTargetCycle (_, _, cyc, _, _, _) = cyc `elem` targetCycles
  extractSS (_, _, _, Just (SignalStrength ss), _, _) = ss
  extractSS (_, _, _, Nothing, _, _) = 0

part2Solution :: Text -> Text
part2Solution = dump . runner . concatMap insToActions . parse
 where
  parse = fromRight [] . runParser (some pInstruction) ""
  runner = reverse . scanr update initialSimState . reverse

  isRenderingCycle (action, _, cyc, _, _, _) =
    cyc `elem` renderingCycles && action == DuringCycle

  extractCRTRow (_, _, _, _, _, CRTRow row) = row

  dump =
    T.intercalate "\n"
      . map (T.pack . extractCRTRow)
      . filter isRenderingCycle

puzzleInput :: Text
puzzleInput =
  T.intercalate
    "\n"
    [ "addx 15"
    , "addx -11"
    , "addx 6"
    , "addx -3"
    , "addx 5"
    , "addx -1"
    , "addx -8"
    , "addx 13"
    , "addx 4"
    , "noop"
    , "addx -1"
    , "addx 5"
    , "addx -1"
    , "addx 5"
    , "addx -1"
    , "addx 5"
    , "addx -1"
    , "addx 5"
    , "addx -1"
    , "addx -35"
    , "addx 1"
    , "addx 24"
    , "addx -19"
    , "addx 1"
    , "addx 16"
    , "addx -11"
    , "noop"
    , "noop"
    , "addx 21"
    , "addx -15"
    , "noop"
    , "noop"
    , "addx -3"
    , "addx 9"
    , "addx 1"
    , "addx -3"
    , "addx 8"
    , "addx 1"
    , "addx 5"
    , "noop"
    , "noop"
    , "noop"
    , "noop"
    , "noop"
    , "addx -36"
    , "noop"
    , "addx 1"
    , "addx 7"
    , "noop"
    , "noop"
    , "noop"
    , "addx 2"
    , "addx 6"
    , "noop"
    , "noop"
    , "noop"
    , "noop"
    , "noop"
    , "addx 1"
    , "noop"
    , "noop"
    , "addx 7"
    , "addx 1"
    , "noop"
    , "addx -13"
    , "addx 13"
    , "addx 7"
    , "noop"
    , "addx 1"
    , "addx -33"
    , "noop"
    , "noop"
    , "noop"
    , "addx 2"
    , "noop"
    , "noop"
    , "noop"
    , "addx 8"
    , "noop"
    , "addx -1"
    , "addx 2"
    , "addx 1"
    , "noop"
    , "addx 17"
    , "addx -9"
    , "addx 1"
    , "addx 1"
    , "addx -3"
    , "addx 11"
    , "noop"
    , "noop"
    , "addx 1"
    , "noop"
    , "addx 1"
    , "noop"
    , "noop"
    , "addx -13"
    , "addx -19"
    , "addx 1"
    , "addx 3"
    , "addx 26"
    , "addx -30"
    , "addx 12"
    , "addx -1"
    , "addx 3"
    , "addx 1"
    , "noop"
    , "noop"
    , "noop"
    , "addx -9"
    , "addx 18"
    , "addx 1"
    , "addx 2"
    , "noop"
    , "noop"
    , "addx 9"
    , "noop"
    , "noop"
    , "noop"
    , "addx -1"
    , "addx 2"
    , "addx -37"
    , "addx 1"
    , "addx 3"
    , "noop"
    , "addx 15"
    , "addx -21"
    , "addx 22"
    , "addx -6"
    , "addx 1"
    , "noop"
    , "addx 2"
    , "addx 1"
    , "noop"
    , "addx -10"
    , "noop"
    , "noop"
    , "addx 20"
    , "addx 1"
    , "addx 2"
    , "addx 2"
    , "addx -6"
    , "addx -11"
    , "noop"
    , "noop"
    , "noop"
    ]
