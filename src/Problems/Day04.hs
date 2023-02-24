module Problems.Day04 (
  part1Solution,
  part2Solution,
) where

import Data.Either (fromRight)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, empty, runParser, some)
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer as L (decimal, lexeme, space)
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
