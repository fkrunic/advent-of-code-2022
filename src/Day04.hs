module Day04 (
  part1Solution,
  part2Solution,
  puzzleInput,
) where

import Data.Either (fromRight)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import Prelude hiding ((<=), (^))

type Parser = Parsec Void Text
type Assignment = (Int, Int)

sc :: Parser ()
sc = L.space space1 empty empty

lexer :: Parser a -> Parser a
lexer = L.lexeme sc

integer :: Parser Int
integer = lexer L.decimal

pAssignment :: Parser Assignment
pAssignment =
  (,) <$> (integer <* char '-') <*> integer

pLine :: Parser (Assignment, Assignment)
pLine =
  (,) <$> (pAssignment <* char ',') <*> pAssignment

isOverlapped :: Assignment -> Assignment -> Bool
isOverlapped (s1, e1) (s2, e2) = (g1 <= g2) || (g2 <= g1)
 where
  r1 = [s1 .. e1]
  r2 = [s2 .. e2]
  g1 = S.fromList r1
  g2 = S.fromList r2
  (<=) = S.isSubsetOf

anyOverlap :: Assignment -> Assignment -> Bool
anyOverlap (s1, e1) (s2, e2) = not $ null $ S.intersection g1 g2
 where
  r1 = [s1 .. e1]
  r2 = [s2 .. e2]
  g1 = S.fromList r1
  g2 = S.fromList r2

part1Solution :: Text -> Int
part1Solution = length . filter (uncurry isOverlapped) . fromRight [] . parser
 where
  parser = runParser (some pLine) ""

part2Solution :: Text -> Int
part2Solution = length . filter (uncurry anyOverlap) . fromRight [] . parser
 where
  parser = runParser (some pLine) ""

puzzleInput :: Text
puzzleInput =
  T.intercalate
    "\n"
    [ "2-4,6-8"
    , "2-3,4-5"
    , "5-7,7-9"
    , "2-8,3-7"
    , "6-6,4-6"
    , "2-6,4-8"
    ]