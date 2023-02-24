module Main where

import Criterion.Main 
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NE
import Day17
import Infinites

target :: Iterations -> Cave
target = towerProcess caveFloor rts winds
  where
    input = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
    winds = case NE.nonEmpty (parse input) of
      Nothing -> error "Cannot parse wind input"
      Just wds -> makeInf wds
    rts = makeInf $ HLine :| [Plus .. Square]     

main :: IO ()
main = defaultMain [
  bgroup "Day 17" [ bench "1" $ whnf target 1 
                  , bench "10" $ whnf target 10
                  , bench "100" $ whnf target 100
                  , bench "1000" $ whnf target 1000
                  ]
  ]