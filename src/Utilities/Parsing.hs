module Utilities.Parsing where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, empty)
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L

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