module Graphs where

import Control.Monad.Trans.State.Strict
import Data.Bifunctor (second)
import Data.Set (Set)
import Data.Set qualified as S

newtype UnvisitedNode a
  = UnvisitedNode (a, TentativeDistance)
  deriving (Show, Eq, Ord)

newtype VisitedNode a
  = VisitedNode (a, TentativeDistance)
  deriving (Show, Eq, Ord)

newtype CurrentNode a
  = CurrentNode (a, TentativeDistance)
  deriving (Show, Eq, Ord)

type UnvisitedSet a = Set (UnvisitedNode a)

data TentativeDistance
  = Infinite
  | Finite Int
  deriving (Show, Eq)

type DjikstraAlgo a = State (CurrentNode a, UnvisitedSet a)

instance Ord TentativeDistance where
  Finite i <= Finite j = i <= j
  Infinite <= Finite _ = False
  Finite _ <= Infinite = True
  Infinite <= Infinite = undefined

toCurrent :: UnvisitedNode a -> CurrentNode a
toCurrent (UnvisitedNode n) = CurrentNode n

makeStartingNode :: a -> UnvisitedNode a
makeStartingNode = UnvisitedNode . (,Finite 0)

makeGenericNode :: a -> UnvisitedNode a
makeGenericNode = UnvisitedNode . (,Infinite)

buildUnvisitedSet :: Ord a => a -> [a] -> UnvisitedSet a
buildUnvisitedSet initial =
  S.union (S.singleton $ makeStartingNode initial)
    . S.fromList
    . map makeGenericNode

remove :: Ord a => a -> Set a -> Set a
remove x s = S.difference s (S.singleton x)

addDistances :: TentativeDistance -> TentativeDistance -> TentativeDistance
addDistances (Finite i) (Finite j) = Finite (i + j)
addDistances _ _ = Infinite

step1And2 :: Ord a => a -> [a] -> DjikstraAlgo a ()
step1And2 initial rest = put (current, unvisited)
 where
  current = toCurrent $ makeStartingNode initial
  unvisited = buildUnvisitedSet initial rest

updateDistances ::
  CurrentNode a ->
  (UnvisitedNode a, TentativeDistance) ->
  UnvisitedNode a
updateDistances (CurrentNode (_, cd)) (uv@(UnvisitedNode (p, td)), nd) =
  if candidate <= td
    then UnvisitedNode (p, candidate)
    else uv
 where
  candidate = addDistances cd nd

step3 ::
  Ord a =>
  (CurrentNode a -> Set (UnvisitedNode a, TentativeDistance)) ->
  DjikstraAlgo a ()
step3 getNeighbors = do
  current <- fst <$> get
  let neighbors = getNeighbors current
      initialUnvisited = S.map fst neighbors
      updatedVisited = S.map (updateDistances current) neighbors
  modify $ second (`S.difference` initialUnvisited)
  modify $ second (`S.union` updatedVisited)
