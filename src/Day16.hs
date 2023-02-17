module Day16 where

import Control.Monad (when)
import Data.Foldable (foldrM)
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Graphs
import Utilities

newtype ValveID = ValveID Text deriving (Show, Eq, Ord)
newtype FlowRate = FlowRate Word deriving (Show, Eq, Ord, Num)
newtype Pressure = Pressure Word deriving (Show, Eq, Ord, Num)
newtype Minutes = Minutes Word deriving (Show, Eq, Ord, Num)

newtype OpenedValves = OpenedValves (Set ValveID) deriving (Show, Eq, Ord)
newtype TunnelValves = TunnelValves (Set ValveID) deriving (Show, Eq, Ord)

data InputLine = InputLine
  { valveID :: ValveID
  , flowRate :: FlowRate
  , tunnels :: TunnelValves
  }
  deriving (Show, Eq)

type FlowMap = Map ValveID FlowRate
type TunnelMap = Map ValveID TunnelValves

newtype PositiveFlowMap = PositiveFlowMap FlowMap deriving (Show, Eq, Ord)
newtype TravelMap = TravelMap (Map TunnelPath Minutes) deriving (Show, Eq)
type MinuteMap = Map ValveID Minutes

newtype TravelMinutes = TravelMinutes Minutes deriving (Show, Eq)
newtype MinutesRemaining = MinutesRemaining Minutes deriving (Show, Eq)

data State = State
  { location :: ValveID
  , openedValves :: OpenedValves
  , timeElapsed :: Minutes
  }
  deriving (Show, Eq)

data CannotTraverseErr = CannotTraverseErr
  { fromValve :: ValveID
  , toValve :: ValveID
  , availableValves :: TunnelValves
  }
  deriving (Show, Eq)

data InfiniteMinutesErr = InfiniteMinutesErr
  { distFromValve :: ValveID
  , distToValve :: ValveID
  }
  deriving (Show, Eq)

data Error
  = ValveAlreadyOpen ValveID
  | UnrecognizedValve ValveID
  | AlreadyAtLocation ValveID
  | CannotTraverse CannotTraverseErr
  | InfiniteMinutes InfiniteMinutesErr
  deriving (Show, Eq)

type Fork = Either Error

data Action
  = DoNothing
  | OpenValve ValveID
  | MoveToValve ValveID
  deriving (Show, Eq)

data Context = Context
  { flowMap :: FlowMap
  , tunnelMap :: TunnelMap
  }
  deriving (Show, Eq)

data TunnelPath = TunnelPath
  { startValve :: ValveID
  , endValve :: ValveID
  }
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------

lookupFlow :: FlowMap -> ValveID -> Fork FlowRate
lookupFlow flows v = note (UnrecognizedValve v) (M.lookup v flows)

lookupTunnels :: TunnelMap -> ValveID -> Fork TunnelValves
lookupTunnels tunnels v = note (UnrecognizedValve v) (M.lookup v tunnels)

totalRelease :: FlowMap -> OpenedValves -> Fork FlowRate
totalRelease flows = fmap sum . mapM (lookupFlow flows) . S.toList . unpack
 where
  unpack (OpenedValves vs) = vs

isTimeExpired :: State -> Bool
isTimeExpired = (>= Minutes 30) . timeElapsed

--------------------------------------------------------------------------------

openValve :: ValveID -> State -> Fork State
openValve valve state@(State _ (OpenedValves opened) time)
  | S.member valve opened = Left (ValveAlreadyOpen valve)
  | otherwise =
      Right $
        state
          { timeElapsed = time + Minutes 1
          , openedValves = OpenedValves $ S.insert valve opened
          }

moveToValve :: ValveID -> TunnelMap -> State -> Fork State
moveToValve target tunnels state@(State loc _ time) = do
  when (target == loc) $ Left (AlreadyAtLocation target)
  tv@(TunnelValves accessible) <- lookupTunnels tunnels loc
  if not (S.member target accessible)
    then
      Left $
        CannotTraverse $
          CannotTraverseErr
            { fromValve = loc
            , toValve = target
            , availableValves = tv
            }
    else
      Right $
        state
          { location = target
          , timeElapsed = time + Minutes 1
          }

doNothing :: State -> Fork State
doNothing state@(State _ _ time) =
  Right $ state{timeElapsed = time + Minutes 1}

--------------------------------------------------------------------------------

applyAction :: Context -> Action -> State -> Fork State
applyAction _ (OpenValve v) = openValve v
applyAction ctx (MoveToValve v) = moveToValve v (tunnelMap ctx)
applyAction _ DoNothing = doNothing

flowEffect :: Context -> Action -> State -> Fork (State, FlowRate)
flowEffect ctx action state = do
  priorFlow <- totalRelease (flowMap ctx) (openedValves state)
  updatedState <- applyAction ctx action state
  return (updatedState, priorFlow)

runActions :: Context -> State -> [Action] -> Fork [(State, FlowRate)]
runActions ctx state =
  fmap reverse . foldrM collector [(state, FlowRate 0)] . reverse
 where
  collector action history =
    let (prior, _) = head history
     in flowEffect ctx action prior >>= Right . (: history)

--------------------------------------------------------------------------------

positiveFlow :: FlowMap -> PositiveFlowMap
positiveFlow = PositiveFlowMap . M.filter (> FlowRate 0)

minuteMap :: ValveID -> TunnelMap -> Fork MinuteMap
minuteMap valve tunnels = M.fromList <$> minutes
 where
  source = Vertex valve
  vertices = map Vertex $ M.keys tunnels
  getEdges (Vertex v) =
    M.fromList . map (\e -> (Vertex e, Finite 1)) . S.toList . unpackTV $
      tunnels ! v
  unpackTV (TunnelValves vs) = vs
  distances = dijkstra source vertices getEdges
  buildMinutes (Vertex neighbor, d) =
    note (InfiniteMinutes (InfiniteMinutesErr valve neighbor)) $
      unpackDistance d >>= Just . (neighbor,) . Minutes
  minutes = mapM buildMinutes $ M.assocs distances

--------------------------------------------------------------------------------

pressure ::
  TravelMinutes ->
  MinutesRemaining ->
  FlowRate ->
  (Pressure, MinutesRemaining)
pressure
  (TravelMinutes (Minutes travel))
  (MinutesRemaining (Minutes current))
  (FlowRate flow) =
    if remaining >= 1
      then ( Pressure $ remaining * flow, MinutesRemaining $ Minutes remaining)
      else (Pressure 0, MinutesRemaining $ Minutes 0)
   where
    remaining = current - travel - 1

--------------------------------------------------------------------------------

{-

Searching rules:
  1. You can only double back if the remaining valves to open require you
      to visit a valve you have previously been to.

  2. If you have multiple choices about which tunnel to choose, pick the
      tunnel leading to the valve with the highest pressure contribution.

  3. Once all the nodes have been visited, stop the search.

-}

{-

Observations about finding the optimal path:
  1. Where you start matters. For example, if you start at AA, you will
      have to double back to AA if you want to open JJ. On the other hand,
      if you started at JJ, you could visit each node once and likely get
      a higher pressure rate.

  2. How nodes are conected matters. If you sever the graph in two, you
      limit your flow rate.

  3. You can have different sequences of actions that yield the optimal flow rate.
    For example, instead of doing nothing at the end, you can move around the
    graph and still end up with the same flow rate.

  4. The valves are not turned on in a monotonic order based on their flow rate.

  5. The valves are not turned on in a monotonic order based on their conditional
    total pressure release starting at time `t`.

  6. The time spacing in between valve openings is not the same.

  7. Closed valves are sometimes skipped to open other ones, and then returned to
      later to be opened. This is particularly true for valves with small flow rates.

-}