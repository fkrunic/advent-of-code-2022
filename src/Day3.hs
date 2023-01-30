{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day3 where

import Data.Char (isLetter, ord)
import Data.Either (fromRight)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import GHC.Utils.Misc (uncurry3)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

lexer :: Parser a -> Parser a
lexer = L.lexeme sc

toSet :: Text -> S.Set Char
toSet = S.fromList . T.unpack

pSack :: Parser (S.Set Char, S.Set Char)
pSack = do
  sack :: Text <- takeWhile1P Nothing isLetter
  let compartmentSize = T.length sack `div` 2
      (firstCompartment, secondCompartment) = T.splitAt compartmentSize sack
  return (toSet firstCompartment, toSet secondCompartment)

pGroup :: Parser (Text, Text, Text)
pGroup =
  (,,) <$> pLine <*> pLine <*> pLine
  where
    pLine = lexer (takeWhile1P Nothing isLetter)

itemInGroup :: Text -> Text -> Text -> Char
itemInGroup t1 t2 t3 =
  head $ S.toList $ toSet t1 ^ toSet t2 ^ toSet t3
  where
    (^) = S.intersection

itemInBoth :: S.Set Char -> S.Set Char -> Char
itemInBoth first second = head $ S.toList $ S.intersection first second

priority :: Char -> Int
priority c
  | c `elem` ['a' .. 'z'] = ord c - 96
  | c `elem` ['A' .. 'Z'] = ord c - 38
  | otherwise = -1

part1Solution :: Text -> Int
part1Solution = sum . map (priority . uncurry itemInBoth) . fromRight [] . parse
  where
    parse = runParser (some (lexer pSack)) ""

part2Solution :: Text -> Int
part2Solution = sum . map (priority . uncurry3 itemInGroup) . fromRight [] . parse
  where
    parse = runParser (some pGroup) ""

puzzleInput :: Text
puzzleInput =
  T.intercalate
    "\n"
    [ "vJrwpWtwJgWrhcsFMMfFFhFp",
      "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
      "PmmdzqPrVvPwwTWBwg",
      "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
      "ttgJtRGJQctTZtZT",
      "CrZsJsPPZsGzwwsLwLmpwMDw"
    ]