module Day14 where

import Grids
import Parsing

import Text.Megaparsec

type DrawPath = [Coordinate]

--------------------------------------------------------------------------------

pVector :: Parser Coordinate
pVector =
  (,)
    <$> (XCoordinate <$> integer <* symbol ",")
    <*> (YCoordinate <$> integer)

pDrawPath :: Parser DrawPath
pDrawPath = some (pVector <* optional (symbol "->"))