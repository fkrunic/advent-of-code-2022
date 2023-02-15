module Day16 where

import Control.Monad (mapM)
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

data InputLine = InputLine
  { valveID :: ValveID
  , flowRate :: FlowRate
  , tunnels :: Set ValveID
  }
  deriving (Show, Eq)

type FlowMap = Map ValveID FlowRate
type OpenedValves = Set ValveID

data State = State
  { location :: ValveID
  , openedValves :: Set ValveID
  , timeElapsed :: Minutes
  }
  deriving (Show, Eq)

data Error
  = ValveAlreadyOpen
  | UnrecognizedValve ValveID
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------

lookupFlow :: FlowMap -> ValveID -> Either Error FlowRate
lookupFlow flows v = note (UnrecognizedValve v) (M.lookup v flows)

totalRelease :: FlowMap -> OpenedValves -> Either Error FlowRate
totalRelease flows = fmap sum . mapM (lookupFlow flows) . S.toList

isTimeExpired :: State -> Bool
isTimeExpired = (>= Minutes 30) . timeElapsed

--------------------------------------------------------------------------------

-- openValve :: ValveID -> State -> State
-- openValve valve state =
--   state{}