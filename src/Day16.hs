module Day16 where

import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Graphs
import Parsing
import System.Random
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

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
type TravelMap = Map ValveID (Maybe TravelMinutes)
type PressureMap = Map ValveID (Maybe (Pressure, MinutesRemaining))
type PressureIndexMap = Map PressureIndex ValveID

newtype TravelMinutes = TravelMinutes Minutes deriving (Show, Eq)
newtype MinutesRemaining = MinutesRemaining Minutes deriving (Show, Eq)

type IndexBuilder =
  FlowMap ->
  TunnelMap ->
  OpenedValves ->
  PressureMap ->
  PressureIndexMap

data Env = Env
  { flowMap :: FlowMap
  , tunnelMap :: TunnelMap
  , indexBuilder :: IndexBuilder
  }

data State = State
  { minutesRemaining :: MinutesRemaining
  , openedValves :: OpenedValves
  }
  deriving (Show, Eq)

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

travelMap :: ValveID -> TunnelMap -> TravelMap
travelMap valve tunnels = M.fromList minutes
 where
  source = Vertex valve
  vertices = map Vertex $ M.keys tunnels
  getEdges (Vertex v) =
    M.fromList . map (\e -> (Vertex e, Finite 1)) . S.toList . unpackTV $
      tunnels ! v
  unpackTV (TunnelValves vs) = vs
  distances = dijkstra source vertices getEdges
  convertDistance d =
    unpackDistance d
      >>= Just . TravelMinutes . Minutes . fromIntegral
  buildMinutes (Vertex neighbor, d) = (neighbor, convertDistance d)
  minutes = map buildMinutes $ M.assocs distances

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
pressureMap remaining flows = M.mapWithKey getPressure
 where
  getPressure valve maybeTM =
    maybeTM >>= \tm -> Just $ pressure tm remaining $ flows ! valve

cumsum :: Num a => a -> [a] -> [a]
cumsum initial =
  tail . reverse . foldr (\e acc -> head acc + e : acc) [initial] . reverse

pressureIndex :: IndexBuilder
pressureIndex _ _ _ pm = M.fromList $ zip indices positiveValves
 where
  positiveChoices =
    M.filter ((> Pressure 0) . fst) $
      M.map fromJust $
        M.filter isJust pm
  indexValues = map (\(Pressure p, _) -> p) $ M.elems positiveChoices
  indices = map PressureIndex $ cumsum 0 indexValues
  positiveValves = M.keys positiveChoices

chooseNextValve ::
  RandomGen g =>
  Env ->
  g ->
  OpenedValves ->
  PressureMap ->
  Maybe (ValveID, g)
chooseNextValve (Env flows tunnels builder) gen opened@(OpenedValves ovs) pm =
  (,nextGen) . snd <$> M.lookupGE pIndex indexMap
 where
  choices = M.withoutKeys pm ovs
  indexMap = builder flows tunnels opened choices
  PressureIndex pMax = maximum $ PressureIndex 1 : M.keys indexMap
  (indexChoice, nextGen) = uniformR (1, pMax) gen
  pIndex = PressureIndex indexChoice

chooseRoute ::
  RandomGen g =>
  Env ->
  g ->
  ValveID ->
  MinutesRemaining ->
  OpenedValves ->
  [(ValveID, Pressure, MinutesRemaining)]
chooseRoute env@(Env flows tunnels _) rand currentValve remainingTime opened = do
  let travel = travelMap currentValve tunnels
  let pm = pressureMap remainingTime flows travel
  case chooseNextValve env rand opened pm of
    Nothing -> []
    Just (nextValve, nextRand) ->
      case pm ! nextValve of
        Nothing -> error $ "Cannot pick empty pressure valve " ++ show nextValve
        Just (ps, nextRemaining) ->
          let nextOpened = OpenedValves $ S.insert nextValve ovs
           in let rest =
                    chooseRoute
                      env
                      nextRand
                      nextValve
                      nextRemaining
                      nextOpened
               in (nextValve, ps, nextRemaining) : rest
 where
  OpenedValves ovs = opened

totalReleased :: [(ValveID, Pressure, MinutesRemaining)] -> Pressure
totalReleased = sum . map (\(_, p, _) -> p)

bestRoute ::
  Env -> 
  NumberOfTrials ->
  ValveID ->
  MinutesRemaining ->
  OpenedValves ->
  Pressure
bestRoute
  env
  (NumberOfTrials trials)
  currentValve
  remainingTime
  opened = maximum $ map (totalReleased . executeTrial) [1 .. trials]
   where
    executeTrial seed =
      chooseRoute
        env
        (mkStdGen $ fromIntegral seed)
        currentValve
        remainingTime
        opened

--------------------------------------------------------------------------------
