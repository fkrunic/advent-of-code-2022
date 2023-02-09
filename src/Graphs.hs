{-# LANGUAGE RecordWildCards #-}

module Graphs where

import Control.Monad.Loops (whileM_)
import Control.Monad.Trans.State.Strict
import Data.Functor ((<&>))
import Data.List (sortBy)
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Void (Void)

data Distance
  = Finite Word
  | Infinite
  deriving (Show, Eq)

newtype Vertex a = Vertex a deriving (Show, Eq, Ord)
type DistanceMap a = Map (Vertex a) Distance
type VoidMap key = Map key Void
type DijkstraAlgo a = State (DijkstraSetup a)
type Edges a = Map (Vertex a, Vertex a) Distance

data DijkstraSetup a = DijkstraSetup
  { dist :: DistanceMap a
  , prev :: DistanceMap a
  , q :: VoidMap (Vertex a)
  }
  deriving (Show, Eq, Ord)

instance Ord Distance where
  Finite i <= Finite j = i <= j
  Finite _ <= Infinite = True
  Infinite <= Finite _ = False
  Infinite <= Infinite = True

modifyDist :: (DistanceMap a -> DistanceMap a) -> DijkstraAlgo a ()
modifyDist f = modify $ \s -> s{dist = f (dist s)}

modifyPrev :: (DistanceMap a -> DistanceMap a) -> DijkstraAlgo a ()
modifyPrev f = modify $ \s -> s{prev = f (prev s)}

modifyQ :: (VoidMap (Vertex a) -> VoidMap (Vertex a)) -> DijkstraAlgo a ()
modifyQ f = modify $ \s -> s{q = f (q s)}

setup :: Ord a => Vertex a -> [Vertex a] -> DijkstraSetup a
setup source vertices = DijkstraSetup{..}
 where
  dist =
    M.adjust (const (Finite 0)) source $
      M.fromList $
        map (,Infinite) vertices
  prev = M.fromList $ map (,undefined) vertices
  q = M.fromList $ map (,undefined) vertices

popMinVertex :: Ord a => DijkstraAlgo a (Vertex a)
popMinVertex = do
  qVertices <- M.keys . q <$> get
  dist' <- dist <$> get
  let distancePairs = map (\v -> (v, dist' ! v)) qVertices
      sortedDistances = sortBy (\(_, d1) (_, d2) -> compare d1 d2) distancePairs
      (u, _) = head sortedDistances
  modifyQ (M.update (const Nothing) u)
  return u

algo :: Edges a -> DijkstraAlgo a (DistanceMap a)
algo edges = do
  whileM_ (get <&> (not . M.null . q)) $ do
    return ()
  dist <$> get