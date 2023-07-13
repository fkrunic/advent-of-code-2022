module Problems.Day02 (
  part1Solution,
  part2Solution,
) where

import Data.Either (fromRight)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, empty, runParser, some)
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

data Outcome = Win | Lose | Draw deriving (Show, Eq)

data Move = Rock | Paper | Scissors deriving (Show, Eq)

scoreMove :: Move -> Int
scoreMove Rock = 1
scoreMove Paper = 2
scoreMove Scissors = 3

scoreOutcome :: Outcome -> Int
scoreOutcome Win = 6
scoreOutcome Lose = 0
scoreOutcome Draw = 3

determineOutcome :: Move -> Move -> Outcome
determineOutcome Rock Paper = Win
determineOutcome Paper Rock = Lose
determineOutcome Rock Scissors = Lose
determineOutcome Scissors Rock = Win
determineOutcome Paper Scissors = Win
determineOutcome Scissors Paper = Lose
determineOutcome _ _ = Draw

moveForOutcome :: Move -> Outcome -> Move
moveForOutcome Rock Win = Paper
moveForOutcome Rock Lose = Scissors
moveForOutcome Paper Win = Scissors
moveForOutcome Paper Lose = Rock
moveForOutcome Scissors Win = Rock
moveForOutcome Scissors Lose = Paper
moveForOutcome move Draw = move

sc :: Parser ()
sc = L.space space1 empty empty

symbol :: Text -> Parser Text
symbol = L.symbol sc

pOpponentMove :: Parser Move
pOpponentMove =
  choice
    [ Rock <$ symbol "A"
    , Paper <$ symbol "B"
    , Scissors <$ symbol "C"
    ]

pPlayerMove :: Parser Move
pPlayerMove =
  choice
    [ Rock <$ symbol "X"
    , Paper <$ symbol "Y"
    , Scissors <$ symbol "Z"
    ]

pOutcome :: Parser Outcome
pOutcome =
  choice
    [ Lose <$ symbol "X"
    , Draw <$ symbol "Y"
    , Win <$ symbol "Z"
    ]

pRound :: Parser (Move, Move)
pRound = (,) <$> pOpponentMove <*> pPlayerMove

pStrategyRound :: Parser (Move, Outcome)
pStrategyRound = (,) <$> pOpponentMove <*> pOutcome

scoreRound :: (Move, Move) -> Int
scoreRound (opponentMove, playerMove) =
  scoreMove playerMove + scoreOutcome outcome
 where
  outcome = determineOutcome opponentMove playerMove

totalScore :: [(Move, Move)] -> Int
totalScore = sum . map scoreRound

part1Solution :: Text -> Int
part1Solution = totalScore . fromRight [] . runParser (some pRound) ""

getMovePair :: (Move, Outcome) -> (Move, Move)
getMovePair (opponentMove, outcome) =
  (opponentMove, moveForOutcome opponentMove outcome)

part2Solution :: Text -> Int
part2Solution =
  totalScore
    . map getMovePair
    . fromRight []
    . runParser (some pStrategyRound) ""
