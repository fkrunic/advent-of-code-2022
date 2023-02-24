module Utilities.Infinites (makeInf, getSplit, Infinite) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE

newtype Infinite a = Infinite (NonEmpty a)

makeInf :: NonEmpty a -> Infinite a
makeInf = Infinite . NE.cycle

getSplit :: Infinite a -> (a, Infinite a)
getSplit (Infinite cyc) =
  case NE.uncons cyc of
    (_, Nothing) -> error "Infinite instance does not have tail elements."
    (front, Just rest) -> (front, Infinite rest)