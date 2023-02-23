module Infinites where

import Data.List.NonEmpty (NonEmpty)

newtype Infinite a = Infinite (NonEmpty a)

getSplit :: Infinite a -> (a, Infinite a)
getSplit = undefined