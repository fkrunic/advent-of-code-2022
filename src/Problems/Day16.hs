module Problems.Day16 where

import Control.Monad.Trans.RWS.CPS hiding (state)
import Data.List (sortBy)
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Utilities.Graphs hiding (path)
import Utilities.Parsing
import System.Random
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

newtype ValveID = ValveID Text deriving (Show, Eq, Ord)
newtype FlowRate = FlowRate Int deriving (Show, Eq, Ord, Num)
newtype Pressure = Pressure Int deriving (Show, Eq, Ord, Num)
newtype PressureIndex = PressureIndex Int deriving (Show, Eq, Ord, Num)
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

type IndexBuilder = PressureMap -> ValveID -> PressureIndex

type Selector =
  forall g.
  RandomGen g =>
  g ->
  Map ValveID PressureIndex ->
  Maybe (ValveID, g)

data Env = Env
  { flowMap :: FlowMap
  , tunnelMap :: TunnelMap
  , indexBuilder :: IndexBuilder
  , valveSelector :: Selector
  }

data State = State
  { minutesRemaining :: MinutesRemaining
  , openedValves :: OpenedValves
  , currentValve :: ValveID
  }
  deriving (Show, Eq)

type VolcanoSim = RWS Env () State

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

optimalRoute :: FlowMap -> TravelMap -> [ValveID]
optimalRoute flows travel = map fst $ filter ((> 0) . snd) rankedValves
 where
  doubleF :: FlowRate -> Double
  doubleF (FlowRate f) = fromInteger $ fromIntegral f

  doubleM :: TravelMinutes -> Double
  doubleM (TravelMinutes (Minutes m)) = fromInteger $ fromIntegral m

  valves = M.keys flows
  efficiencies =
    map (\v -> (v, doubleF (flows ! v) / doubleM (fromJust $ travel ! v))) valves

  rankedValves = sortBy (\(_, e1) (_, e2) -> compare e2 e1) efficiencies

trip :: ValveID -> FlowMap -> TunnelMap -> [(ValveID, Pressure, MinutesRemaining)]
trip startV flows tunnels =
  tail $ reverse $ foldr builder [initialState] (reverse route)
 where
  initialState = (startV, Pressure 0, MinutesRemaining $ Minutes 30)
  route = optimalRoute flows (travelMap startV tunnels)
  builder nextValve history = (nextValve, p, updatedRemaining) : history
   where
    (priorValve, _, remaining) = head history
    fromPriorToNextTravel = travelMap priorValve tunnels
    commute = fromJust $ fromPriorToNextTravel ! nextValve
    (p, updatedRemaining) = pressure commute remaining (flows ! nextValve)

tripReleased :: [(ValveID, Pressure, MinutesRemaining)] -> Pressure
tripReleased = sum . map (\(_, p, _) -> p)

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

chooseNextValve ::
  RandomGen g =>
  g ->
  IndexBuilder ->
  Selector ->
  OpenedValves ->
  PressureMap ->
  Maybe (ValveID, g)
chooseNextValve gen builder selector (OpenedValves ovs) pm =
  selector gen indexValues
 where
  nonOpenedValves = M.withoutKeys pm ovs
  indexValues = M.mapWithKey (\valve _ -> builder pm valve) nonOpenedValves

simulate :: RandomGen g => g -> VolcanoSim [(ValveID, Pressure, MinutesRemaining)]
simulate rand = do
  State remainingTime opened@(OpenedValves ovs) current <- get
  Env flows tunnels builder selector <- ask
  let travel = travelMap current tunnels
      pm = pressureMap remainingTime flows travel
  case chooseNextValve rand builder selector opened pm of
    Nothing -> pure []
    Just (nextValve, nextRand) ->
      case pm ! nextValve of
        Nothing -> error $ "Cannot pick empty pressure valve " ++ show nextValve
        Just (ps, nextRemaining) -> do
          let nextOpened = OpenedValves $ S.insert nextValve ovs
          modify $ \s -> s{openedValves = nextOpened}
          modify $ \s -> s{currentValve = nextValve}
          modify $ \s -> s{minutesRemaining = nextRemaining}
          rest <- simulate nextRand
          return $ (nextValve, ps, nextRemaining) : rest

totalReleased :: [(ValveID, Pressure, MinutesRemaining)] -> Pressure
totalReleased = sum . map (\(_, p, _) -> p)

bestRoute :: Env -> State -> NumberOfTrials -> Pressure
bestRoute env state (NumberOfTrials trials) =
  maximum $ map (totalReleased . fst . executeTrial) [1 .. trials]
 where
  executeTrial seed =
    evalRWS (simulate (mkStdGen $ fromIntegral seed)) env state

--------------------------------------------------------------------------------

simpleIndex :: IndexBuilder
simpleIndex pm valve = maybe (PressureIndex 0) (convert . fst) (pm ! valve)
 where
  convert (Pressure p) = PressureIndex p

combinedIndex :: IndexBuilder
combinedIndex pm valve = maybe (PressureIndex 0) combine (pm ! valve)
 where
  combine (Pressure p, MinutesRemaining (Minutes m)) = PressureIndex (p * m)

constantIndex :: IndexBuilder
constantIndex pm valve =
  case pm ! valve of
    Nothing -> PressureIndex 0
    Just (Pressure p, _) ->
      if p == 0
        then PressureIndex 0
        else PressureIndex 10

releaseIndex :: MinutesRemaining -> IndexBuilder
releaseIndex (MinutesRemaining (Minutes base)) pm valve =
  case pm ! valve of
    Nothing -> PressureIndex 0
    Just (Pressure p, MinutesRemaining (Minutes m)) ->
      if p == 0
        then PressureIndex 0
        else PressureIndex $ p `div` (base - m)

mixedIndex :: MinutesRemaining -> IndexBuilder
mixedIndex mr pm valve = 0 * simpleIndex pm valve + 10 * releaseIndex mr pm valve

-- Pressure 500, Minutes remaining 20
-- Pressure 500, Minutes remaining 29

flowEfficiencyIndex :: MinutesRemaining -> IndexBuilder
flowEfficiencyIndex (MinutesRemaining (Minutes base)) pm valve =
  case pm ! valve of
    Nothing -> PressureIndex 0
    Just (Pressure p, MinutesRemaining (Minutes m)) ->
      let flowRate = p `div` (m + 1)
       in let efficiency = convertD flowRate / convertD (base - m + 1)
           in PressureIndex $ round $ 1000 * efficiency
 where
  convertD :: Int -> Double
  convertD = fromInteger . fromIntegral

exclusionIndex :: FlowMap -> IndexBuilder
exclusionIndex flows pm valve =
  case pm ! valve of
    Nothing -> PressureIndex 0
    Just (Pressure p, mr) ->
      if p == 0
        then PressureIndex 0
        else PressureIndex $ p + averageAltFlow * unpackMR mr
 where
  otherValves = S.fromList $ filter (/= valve) $ M.keys pm
  otherFlows = M.withoutKeys flows otherValves
  flowTotal = sum $ map unpackFR $ M.elems otherFlows
  flowCount = length $ M.elems otherFlows
  averageAltFlow = flowTotal `div` flowCount
  unpackMR (MinutesRemaining (Minutes m)) = m
  unpackFR (FlowRate f) = f

uniformIndexSelector :: Selector
uniformIndexSelector gen indexMap =
  (,nextGen) . snd
    <$> M.lookupGE pIndex reverseMap
 where
  positiveIndices = M.filter (> PressureIndex 0) indexMap
  cumulativeIndices = cumsum (PressureIndex 0) $ M.elems positiveIndices
  reverseMap = M.fromList $ zip cumulativeIndices (M.keys positiveIndices)
  PressureIndex pMax = maximum $ PressureIndex 1 : M.keys reverseMap
  (indexChoice, nextGen) = uniformR (1, pMax) gen
  pIndex = PressureIndex indexChoice

--------------------------------------------------------------------------------

-- routePressure :: FlowMap -> Route -> Pressure
-- routePressure flows route = foldr addPressure (Pressure 0) (path route)
--  where
--   addPressure (valve, MinutesRemaining (Minutes m)) total =
--     case flows ! valve of
--       FlowRate f -> Pressure (f * m) + total

makeAtlas :: FlowMap -> TunnelMap -> Atlas
makeAtlas flows tunnels = M.fromList atlasPairs
 where
  valves :: [ValveID] = M.keys flows
  atlasPairs = map (\v -> (v, enhanceTravel $ travelMap v tunnels)) valves

  addFlow :: (ValveID, Maybe TravelMinutes) -> (ValveID, (FlowRate, TravelMinutes))
  addFlow (valve, travelTime) = (valve, (flows ! valve, fromJust travelTime))

  enhanceTravel :: TravelMap -> Map ValveID (FlowRate, TravelMinutes)
  enhanceTravel = M.fromList . map addFlow . M.assocs

data Route = Route
  { routePath :: [(ValveID, MinutesRemaining)]
  , remaining :: MinutesRemaining
  , opened :: OpenedValves
  , routePressure :: Pressure
  }
  deriving (Show, Eq)  

addValveToRoute :: Route -> (ValveID, (FlowRate, TravelMinutes)) -> Maybe Route
addValveToRoute
  (Route routePath (MinutesRemaining r) (OpenedValves ovs) routePressure)
  (valve, (FlowRate f, TravelMinutes m))
    | remainingMinutes > 0 && not (S.member valve ovs) =
        Just $
          Route
            { routePath = (valve, MinutesRemaining remainingMinutes) : routePath
            , remaining = MinutesRemaining remainingMinutes
            , opened = OpenedValves $ S.insert valve ovs
            , routePressure = Pressure (f * rm) + routePressure
            }
    | otherwise = Nothing
   where
    remainingMinutes@(Minutes rm) = r - m - 1

type Atlas = Map ValveID (Map ValveID (FlowRate, TravelMinutes))

findMaxPressure ::
  Atlas ->
  [Route] ->
  Pressure ->
  Pressure
findMaxPressure _ [] maxPressure = maxPressure
findMaxPressure atlas (route : rest) maxPressure =
  findMaxPressure atlas (nextRoutes ++ rest) updatedMaxPressure
 where
  valve :: ValveID = fst $ head $ routePath route
  travelOptions :: Map ValveID (FlowRate, TravelMinutes) = atlas ! valve
  nextRoutes :: [Route] =
    mapMaybe (addValveToRoute route) $
      M.assocs travelOptions
  updatedMaxPressure = max (routePressure route) maxPressure
