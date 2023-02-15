module Day16 where

import Control.Monad (mapM, when)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
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
  , tunnels :: OpenedValves
  }
  deriving (Show, Eq)

type FlowMap = Map ValveID FlowRate
type TunnelMap = Map ValveID TunnelValves

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

data Error
  = ValveAlreadyOpen ValveID
  | UnrecognizedValve ValveID
  | AlreadyAtLocation ValveID
  | CannotTraverse CannotTraverseErr
  deriving (Show, Eq)

type Action = Either Error

--------------------------------------------------------------------------------

lookupFlow :: FlowMap -> ValveID -> Action FlowRate
lookupFlow flows v = note (UnrecognizedValve v) (M.lookup v flows)

lookupTunnels :: TunnelMap -> ValveID -> Action TunnelValves
lookupTunnels tunnels v = note (UnrecognizedValve v) (M.lookup v tunnels)

totalRelease :: FlowMap -> OpenedValves -> Action FlowRate
totalRelease flows = fmap sum . mapM (lookupFlow flows) . S.toList . unpack
 where
  unpack (OpenedValves vs) = vs

isTimeExpired :: State -> Bool
isTimeExpired = (>= Minutes 30) . timeElapsed

--------------------------------------------------------------------------------

openValve :: ValveID -> State -> Action State
openValve valve state@(State _ (OpenedValves opened) time)
  | S.member valve opened = Left (ValveAlreadyOpen valve)
  | otherwise =
      Right $
        state
          { timeElapsed = time + Minutes 1
          , openedValves = OpenedValves $ S.insert valve opened
          }

moveToValve :: ValveID -> TunnelMap -> State -> Action State
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