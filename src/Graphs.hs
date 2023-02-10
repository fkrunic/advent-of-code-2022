{-# LANGUAGE RecordWildCards #-}

module Graphs where

import Control.Monad (forM_, when)
import Control.Monad.Loops (whileM_)
import Control.Monad.Trans.State.Strict
    ( State, execState, get, modify )
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.List (sortBy)
import Data.Map (Map, (!))
import Data.Map qualified as M
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
type TrimmedGraph a = Map (Vertex a) (Vertex a)

data DijkstraSetup a = DijkstraSetup
  { dist :: DistanceMap a
  , prev :: TrimmedGraph a
  , q :: VoidMap (Vertex a)
  }
  deriving (Show, Eq, Ord)

instance Ord Distance where
  Finite i <= Finite j = i <= j
  Finite _ <= Infinite = True
  Infinite <= Finite _ = False
  Infinite <= Infinite = True

addDistances :: Distance -> Distance -> Distance
addDistances (Finite i) (Finite j) = Finite (i + j)
addDistances _ _ = Infinite

modifyDist :: (DistanceMap a -> DistanceMap a) -> DijkstraAlgo a ()
modifyDist f = modify $ \s -> s{dist = f (dist s)}

modifyPrev :: (TrimmedGraph a -> TrimmedGraph a) -> DijkstraAlgo a ()
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

setupForMultipleSources ::
  Ord a => [Vertex a] -> [Vertex a] -> DijkstraSetup a
setupForMultipleSources sources vertices = DijkstraSetup{..}
 where
  dist =
    M.mapWithKey setDistance $ M.fromList $ map (,Infinite) vertices
  prev = M.fromList $ map (,undefined) vertices
  q = M.fromList $ map (,undefined) vertices
  setDistance source d =
    if source `elem` sources
      then Finite 0
      else d

popMinVertex :: Ord a => DijkstraAlgo a (Vertex a)
popMinVertex = do
  qVertices <- M.keys . q <$> get
  dist' <- dist <$> get
  let distancePairs = map (\v -> (v, dist' ! v)) qVertices
      sortedDistances = sortBy (\(_, d1) (_, d2) -> compare d1 d2) distancePairs
      (u, _) = head sortedDistances
  modifyQ (M.update (const Nothing) u)
  return u

algo ::
  Ord a =>
  (Vertex a -> Vertex a -> Distance) ->
  (Vertex a -> [Vertex a]) ->
  DijkstraAlgo a ()
algo getDistance getNeighbors = do
  whileM_ (get <&> (not . M.null . q)) $ do
    u <- popMinVertex
    unvisited <- S.fromList . M.keys . q <$> get
    let neighbors = getNeighbors u
        unvisitedNeighbors = S.intersection (S.fromList neighbors) unvisited
    forM_ unvisitedNeighbors $ \v -> do
      dist' <- dist <$> get
      let alt = addDistances (dist' ! u) (getDistance u v)
      when (alt < dist' ! v) $ do
        modifyDist $ M.adjust (const alt) v
        modifyPrev $ M.adjust (const u) v

dijkstra ::
  Ord a =>
  Vertex a ->
  [Vertex a] ->
  (Vertex a -> Vertex a -> Distance) ->
  (Vertex a -> [Vertex a]) ->
  DistanceMap a
dijkstra source vertices getDistance getNeighbors =
  dist $ execState (algo getDistance getNeighbors) setupAlgo
 where
  setupAlgo = setup source vertices

dijkstraMultipleSources ::
  Ord a =>
  [Vertex a] ->
  [Vertex a] ->
  (Vertex a -> Vertex a -> Distance) ->
  (Vertex a -> [Vertex a]) ->
  DistanceMap a
dijkstraMultipleSources sources vertices getDistance getNeighbors =
  dist $ execState (algo getDistance getNeighbors) multiSetup
 where
  multiSetup = setupForMultipleSources sources vertices

--------------------------------------------------------------------------------

unpackDistance :: Distance -> Maybe Word
unpackDistance (Finite w) = Just w
unpackDistance Infinite = Nothing

unpackVertex :: Vertex a -> a
unpackVertex (Vertex a) = a

extractTargetDistance :: (a -> Bool) -> DistanceMap a -> Maybe Word
extractTargetDistance p =
  unpackDistance
    . snd
    . head
    . filter (p . fst)
    . map (first unpackVertex)
    . M.assocs
