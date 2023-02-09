{-# LANGUAGE RecordWildCards #-}

module Graphs where

import Control.Monad.Trans.State.Strict
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Void (Void)

data Distance
  = Finite Word
  | Infinite
  deriving (Show, Eq, Ord)

newtype Vertex a = Vertex a deriving (Show, Eq, Ord)
type DistanceMap a = Map (Vertex a) Distance
type VoidMap key = Map key Void

data DijkstraSetup a = DijkstraSetup
  { dist :: DistanceMap a
  , prev :: DistanceMap a
  , q :: VoidMap (Vertex a)
  }
  deriving (Show, Eq, Ord)

setup :: Ord a => Vertex a -> [Vertex a] -> DijkstraSetup a
setup source vertices = DijkstraSetup{..}
 where
  dist =
    M.adjust (const (Finite 0)) source $
      M.fromList $
        map (,Infinite) vertices
  prev = M.fromList $ map (,undefined) vertices
  q = M.fromList $ map (,undefined) vertices

  