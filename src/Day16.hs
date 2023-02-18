module Day16 where

import Control.Monad (forM)
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Graphs
import Parsing
import System.Random
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Utilities

newtype ValveID = ValveID Text deriving (Show, Eq, Ord)
newtype FlowRate = FlowRate Int deriving (Show, Eq, Ord, Num)
newtype Pressure = Pressure Int deriving (Show, Eq, Ord, Num)
newtype PressureIndex = PressureIndex Int deriving (Show, Eq, Ord)
newtype PressureRange = PressureRange Pressure deriving (Show, Eq)
newtype Minutes = Minutes Int deriving (Show, Eq, Ord, Num)
newtype NumberOfTrials = NumberOfTrials Word deriving (Show, Eq)

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
type TravelMap = Map ValveID TravelMinutes
type PressureMap = Map ValveID (Pressure, MinutesRemaining)
type PressureIndexMap = Map PressureIndex ValveID

newtype TravelMinutes = TravelMinutes Minutes deriving (Show, Eq)
newtype MinutesRemaining = MinutesRemaining Minutes deriving (Show, Eq)

data InfiniteMinutesErr = InfiniteMinutesErr
  { distFromValve :: ValveID
  , distToValve :: ValveID
  }
  deriving (Show, Eq)

data Error
  = UnrecognizedValve ValveID
  | InfiniteMinutes InfiniteMinutesErr
  deriving (Show, Eq)

type Fork = Either Error

type IndexBuilder =
  FlowMap ->
  TunnelMap ->
  OpenedValves ->
  PressureMap ->
  PressureIndexMap

--------------------------------------------------------------------------------

pValveID :: Parser ValveID
pValveID = ValveID . T.pack <$> (symbol "Valve " *> some upperChar)

pFlowRate :: Parser FlowRate
pFlowRate =
  FlowRate
    <$> (symbol " has flow rate=" *> integer <* symbol ";")

plurals :: Text -> Parser ()
plurals t = symbol t *> optional (char 's') *> space

pTunnelValves :: Parser TunnelValves
pTunnelValves =
  TunnelValves . S.fromList . map (ValveID . T.pack)
    <$> ( plurals "tunnel"
            *> plurals "lead"
            *> symbol "to"
            *> plurals "valve"
            *> some (some upperChar <* optional (symbol ","))
        )

pLine :: Parser InputLine
pLine = InputLine <$> pValveID <*> pFlowRate <*> pTunnelValves

--------------------------------------------------------------------------------

travelMap :: ValveID -> TunnelMap -> Fork TravelMap
travelMap valve tunnels = M.fromList <$> minutes
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
      unpackDistance d
        >>= Just . (neighbor,) . TravelMinutes . Minutes . fromIntegral
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
      then (Pressure $ remaining * flow, MinutesRemaining $ Minutes remaining)
      else (Pressure 0, MinutesRemaining $ Minutes 0)
   where
    remaining = current - travel - 1

pressureMap ::
  MinutesRemaining ->
  FlowMap ->
  TravelMap ->
  PressureMap
pressureMap remaining flows = M.fromList . map getPressure . M.assocs
 where
  getPressure (valve, tm) =
    (valve,) $ pressure tm remaining $ flows ! valve

-- note (UnrecognizedValve valve) $
-- M.lookup valve flows >>= Just . (valve,) . pressure tm remaining

cumsum :: Num a => a -> [a] -> [a]
cumsum initial =
  tail . reverse . foldr (\e acc -> head acc + e : acc) [initial] . reverse

pressureIndex :: IndexBuilder
pressureIndex _ _ _ pm = M.fromList $ zip indices positiveValves
 where
  positiveChoices = M.filter ((> Pressure 0) . fst) pm
  indexValues = map (\(Pressure p, _) -> p) $ M.elems positiveChoices
  indices = map PressureIndex $ cumsum 0 indexValues
  positiveValves = M.keys positiveChoices

chooseNextValve ::
  RandomGen g =>
  IndexBuilder ->
  FlowMap ->
  TunnelMap ->
  g ->
  OpenedValves ->
  PressureMap ->
  Maybe (ValveID, g)
chooseNextValve builder flows tunnels gen opened@(OpenedValves ovs) pm =
  (,nextGen) . snd <$> M.lookupGE pIndex indexMap
 where
  choices = M.withoutKeys pm ovs
  indexMap = builder flows tunnels opened choices
  PressureIndex pMax = maximum $ PressureIndex 1 : M.keys indexMap
  (indexChoice, nextGen) = uniformR (1, pMax) gen
  pIndex = PressureIndex indexChoice

chooseRoute ::
  RandomGen g =>
  IndexBuilder ->
  FlowMap ->
  TunnelMap ->
  g ->
  ValveID ->
  MinutesRemaining ->
  OpenedValves ->
  Fork [(ValveID, Pressure, MinutesRemaining)]
chooseRoute builder flows tunnels rand currentValve remainingTime opened = do
  travel <- travelMap currentValve tunnels
  let pm = pressureMap remainingTime flows travel
  case chooseNextValve builder flows tunnels rand opened pm of
    Nothing -> Right []
    Just (nextValve, nextRand) -> do
      (ps, nextRemaining) <-
        note (UnrecognizedValve nextValve) $
          M.lookup nextValve pm
      let nextOpened = OpenedValves $ S.insert nextValve ovs
      rest <-
        chooseRoute
          builder
          flows
          tunnels
          nextRand
          nextValve
          nextRemaining
          nextOpened
      return $ (nextValve, ps, nextRemaining) : rest
 where
  OpenedValves ovs = opened

totalReleased :: [(ValveID, Pressure, MinutesRemaining)] -> Pressure
totalReleased = sum . map (\(_, p, _) -> p)

bestRoute ::
  NumberOfTrials ->
  IndexBuilder ->
  FlowMap ->
  TunnelMap ->
  ValveID ->
  MinutesRemaining ->
  OpenedValves ->
  Fork Pressure
bestRoute
  (NumberOfTrials trials)
  builder
  flows
  tunnels
  currentValve
  remainingTime
  opened =
    fmap maximum $ forM [1 .. trials] $ \seed ->
      totalReleased
        <$> chooseRoute
          builder
          flows
          tunnels
          (mkStdGen $ fromIntegral seed)
          currentValve
          remainingTime
          opened

--------------------------------------------------------------------------------
