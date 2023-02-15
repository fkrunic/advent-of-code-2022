module Day16 where

import Control.Monad (when)
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
  tv@(TunnelValves accessible) <- lookupTunnels tunnels target
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

--------------------------------------------------------------------------------

applyAction :: Context -> Action -> State -> Fork State
applyAction _ (OpenValve v) = openValve v
applyAction ctx (MoveToValve v) = moveToValve v (tunnelMap ctx)
applyAction _ DoNothing = Right

flowEffect :: Context -> Action -> State -> Fork (State, FlowRate)
flowEffect ctx action state = do
  priorFlow <- totalRelease (flowMap ctx) (openedValves state)
  updatedState <- applyAction ctx action state
  return (updatedState, priorFlow)

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
