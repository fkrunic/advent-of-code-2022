module Test.Day16Spec (spec) where

import Control.Monad
import Control.Monad.Trans.RWS.CPS hiding (get, put)
import Control.Monad.Trans.State.Strict hiding (State)
import Data.Either (fromRight)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Problems.Day16
import System.Random
import Text.Megaparsec hiding (State, choice)
import Text.Megaparsec.Char

import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec =
  testGroup "Day 16 Tests" $
    [ parsingTests
    , miscTests
    , choosingValveTests
    , solutionTests
    ]

parsingTests :: TestTree
parsingTests = 
  testGroup "Parsing Tests"
    [ testCase "Parses Example Input" $
        let expected =
              [ InputLine (ValveID "AA") (FlowRate 0) (makeTVS ["DD", "II", "BB"])
              , InputLine (ValveID "BB") (FlowRate 13) (makeTVS ["CC", "AA"])
              , InputLine (ValveID "CC") (FlowRate 2) (makeTVS ["DD", "BB"])
              , InputLine (ValveID "DD") (FlowRate 20) (makeTVS ["CC", "AA", "EE"])
              , InputLine (ValveID "EE") (FlowRate 3) (makeTVS ["FF", "DD"])
              , InputLine (ValveID "FF") (FlowRate 0) (makeTVS ["EE", "GG"])
              , InputLine (ValveID "GG") (FlowRate 0) (makeTVS ["FF", "HH"])
              , InputLine (ValveID "HH") (FlowRate 22) (makeTVS ["GG"])
              , InputLine (ValveID "II") (FlowRate 0) (makeTVS ["AA", "JJ"])
              , InputLine (ValveID "JJ") (FlowRate 21) (makeTVS ["II"])
              ]
            actual =
              fromRight [] $
                runParser (some (pLine <* optional newline)) "" exampleInput
        in actual @?= expected
    ]
    
miscTests :: TestTree
miscTests = 
  testGroup "Miscellaneous Tests" $
    [ testCase "Cumulative Sums" $
        let ps =
              map
                Pressure
                [ 0
                , 364
                , 54
                , 560
                , 81
                , 0
                , 0
                , 528
                , 0
                , 567
                ]
            expected =
              map
                Pressure
                [ 0
                , 364
                , 418
                , 978
                , 1059
                , 1059
                , 1059
                , 1587
                , 1587
                , 2154
                ]
        in cumsum (Pressure 0) ps @?= expected
    , testCase "Minute Maps" $
        let expected =
              M.fromList
                [ (ValveID "AA", Just $ TravelMinutes $ Minutes 0)
                , (ValveID "BB", Just $ TravelMinutes $ Minutes 1)
                , (ValveID "CC", Just $ TravelMinutes $ Minutes 2)
                , (ValveID "DD", Just $ TravelMinutes $ Minutes 1)
                , (ValveID "EE", Just $ TravelMinutes $ Minutes 2)
                , (ValveID "FF", Just $ TravelMinutes $ Minutes 3)
                , (ValveID "GG", Just $ TravelMinutes $ Minutes 4)
                , (ValveID "HH", Just $ TravelMinutes $ Minutes 5)
                , (ValveID "II", Just $ TravelMinutes $ Minutes 1)
                , (ValveID "JJ", Just $ TravelMinutes $ Minutes 2)
                ]
            actual = travelMap (ValveID "AA") (tunnelMap env)
        in actual @?= expected
  , testCase "Pressure Maps" $
      let minutesRemaining = MinutesRemaining $ Minutes 30
          travelM = travelMap (ValveID "AA") (tunnelMap env)
          actual = pressureMap minutesRemaining (flowMap env) travelM
          expected =
            M.fromList
              [ (ValveID "AA", Just (Pressure 0, MinutesRemaining $ Minutes 29))
              , (ValveID "BB", Just (Pressure 364, MinutesRemaining $ Minutes 28))
              , (ValveID "CC", Just (Pressure 54, MinutesRemaining $ Minutes 27))
              , (ValveID "DD", Just (Pressure 560, MinutesRemaining $ Minutes 28))
              , (ValveID "EE", Just (Pressure 81, MinutesRemaining $ Minutes 27))
              , (ValveID "FF", Just (Pressure 0, MinutesRemaining $ Minutes 26))
              , (ValveID "GG", Just (Pressure 0, MinutesRemaining $ Minutes 25))
              , (ValveID "HH", Just (Pressure 528, MinutesRemaining $ Minutes 24))
              , (ValveID "II", Just (Pressure 0, MinutesRemaining $ Minutes 28))
              , (ValveID "JJ", Just (Pressure 567, MinutesRemaining $ Minutes 27))
              ]
      in actual @?= expected

  , testCase "Pressure Tests" $
      let released =
            pressure
              (TravelMinutes (Minutes 1))
              (MinutesRemaining (Minutes 30))
              (FlowRate 13)
      in released @?= (Pressure 364, MinutesRemaining (Minutes 28))
    ]

choosingValveTests :: TestTree
choosingValveTests = 
    testGroup "Choosing Valves" $
      [ testCase "One positive valve" $
          let rand = mkStdGen 42
              opened = OpenedValves S.empty
              pressures =
                M.singleton
                  (ValveID "A")
                  (Just (Pressure 1, MinutesRemaining $ Minutes 1))
              choice = chooseNextValve rand simpleIndex uniformIndexSelector opened pressures
          in fst <$> choice @?= Just (ValveID "A")

  , testCase "No positive valves" $
        let rand = mkStdGen 42
            opened = OpenedValves S.empty
            pressures =
              M.singleton
                (ValveID "A")
                (Just (Pressure 0, MinutesRemaining $ Minutes 1))
            choice = chooseNextValve rand simpleIndex uniformIndexSelector opened pressures
        in choice @?= Nothing

  ,  testCase "Only one positive valve and the rest zero" $
        let rand = mkStdGen 42
            opened = OpenedValves S.empty
            pressures =
              M.fromList
                [ (ValveID "A", Just (Pressure 0, MinutesRemaining $ Minutes 1))
                , (ValveID "B", Just (Pressure 0, MinutesRemaining $ Minutes 1))
                , (ValveID "C", Just (Pressure 1, MinutesRemaining $ Minutes 1))
                , (ValveID "D", Just (Pressure 0, MinutesRemaining $ Minutes 1))
                , (ValveID "E", Just (Pressure 0, MinutesRemaining $ Minutes 1))
                , (ValveID "F", Just (Pressure 0, MinutesRemaining $ Minutes 1))
                ]
            choice = chooseNextValve rand simpleIndex uniformIndexSelector opened pressures
        in fst <$> choice @?= Just (ValveID "C")

  , testCase "Only choosing positive valves" $
        let opened = OpenedValves S.empty
            pressures =
              M.fromList
                [ (ValveID "A", Just (Pressure 0, MinutesRemaining $ Minutes 1))
                , (ValveID "B", Just (Pressure 0, MinutesRemaining $ Minutes 1))
                , (ValveID "C", Just (Pressure 10, MinutesRemaining $ Minutes 1))
                , (ValveID "D", Just (Pressure 0, MinutesRemaining $ Minutes 1))
                , (ValveID "E", Just (Pressure 10, MinutesRemaining $ Minutes 1))
                , (ValveID "F", Just (Pressure 3, MinutesRemaining $ Minutes 1))
                ]

            choices = sequence $ flip evalState (mkStdGen 42) $
              forM [1 :: Int .. 100] $ \_ -> do
                g <- get
                case chooseNextValve g simpleIndex uniformIndexSelector opened pressures of
                  Nothing -> return Nothing
                  Just (valve, g') -> do
                    put g'
                    return (Just valve)
            expectedValves = S.fromList $ map ValveID ["C", "E", "F"]
            actual = S.fromList <$> choices
        in actual @?= Just expectedValves

  , testCase "Only choosing positive valves in proportional ratios" $
        let opened = OpenedValves S.empty
            pressures =
              M.fromList
                [ (ValveID "A", Just (Pressure 0, MinutesRemaining $ Minutes 1))
                , (ValveID "B", Just (Pressure 0, MinutesRemaining $ Minutes 1))
                , (ValveID "C", Just (Pressure 100, MinutesRemaining $ Minutes 1))
                , (ValveID "D", Just (Pressure 0, MinutesRemaining $ Minutes 1))
                , (ValveID "E", Just (Pressure 0, MinutesRemaining $ Minutes 1))
                , (ValveID "F", Just (Pressure 1, MinutesRemaining $ Minutes 1))
                ]
            choices = sequence $ flip evalState (mkStdGen 42) $
              forM [1 :: Int .. 10000] $ \_ -> do
                g <- get
                case chooseNextValve g simpleIndex uniformIndexSelector opened pressures of
                  Nothing -> return Nothing
                  Just (valve, g') -> do
                    put g'
                    return (Just valve)
            numChoiceC = length . filter (== ValveID "C") $ fromJust choices
            numChoiceF = length . filter (== ValveID "F") $ fromJust choices
        in do 
          numChoiceF @?= 94
          numChoiceC @?= 9906

  ,  testCase "Can choose a route" $
        let actualV1 = fst $ evalRWS (simulate (mkStdGen 42)) env initialState
            expectedV1 =
              [ (ValveID "DD", Pressure 560, MinutesRemaining (Minutes 28))
              , (ValveID "HH", Pressure 506, MinutesRemaining (Minutes 23))
              , (ValveID "BB", Pressure 208, MinutesRemaining (Minutes 16))
              , (ValveID "JJ", Pressure 252, MinutesRemaining (Minutes 12))
              , (ValveID "EE", Pressure 21, MinutesRemaining (Minutes 7))
              , (ValveID "CC", Pressure 8, MinutesRemaining (Minutes 4))
              ]
            actualV2 = fst $ evalRWS (simulate (mkStdGen 14)) env initialState
            expectedV2 =
              [ (ValveID "HH", Pressure 528, MinutesRemaining (Minutes 24))
              , (ValveID "DD", Pressure 380, MinutesRemaining (Minutes 19))
              , (ValveID "JJ", Pressure 315, MinutesRemaining (Minutes 15))
              , (ValveID "BB", Pressure 143, MinutesRemaining (Minutes 11))
              , (ValveID "EE", Pressure 21, MinutesRemaining (Minutes 7))
              , (ValveID "CC", Pressure 8, MinutesRemaining (Minutes 4))
              ]
        in do 
          actualV1 @?= expectedV1
          actualV2 @?= expectedV2

  ,  testCase "Can find the best route" $
        let actual = bestRoute env initialState (NumberOfTrials 1000)
            expected = Pressure 1651
        in actual @?= expected

  ,  testCase "It can find the best route without simulation" $
        let travel = travelMap (ValveID "AA") (tunnelMap env)
            actual = optimalRoute (flowMap env) travel
            expected = map ValveID ["DD", "BB", "JJ", "HH", "EE", "CC"]
        in actual @?= expected

  ,  testCase "Can calculate the correct trip" $
        let actual = trip (ValveID "AA") (flowMap env) (tunnelMap env)
            expected =
              [ (ValveID "DD", Pressure 560, MinutesRemaining (Minutes 28))
              , (ValveID "BB", Pressure 325, MinutesRemaining (Minutes 25))
              , (ValveID "JJ", Pressure 441, MinutesRemaining (Minutes 21))
              , (ValveID "HH", Pressure 286, MinutesRemaining (Minutes 13))
              , (ValveID "EE", Pressure 27, MinutesRemaining (Minutes 9))
              , (ValveID "CC", Pressure 12, MinutesRemaining (Minutes 6))
              ]
        in do 
          actual @?= expected
          tripReleased actual @?= 1651
      ]

solutionTests :: TestTree
solutionTests = 
  testGroup "Puzzle Solutions" $
    [ testCase "Part 1 Solution - Example Input" $
          let actual = part1Solution exampleInput
              expected = Pressure 1651
          in actual @?= expected

    , testCase "Part 1 Solution - Puzzle Input" $
          let actual = part1Solution puzzleInput
              expected = Pressure 1
          in do 
            "Never terminates" @?= "Finishes"
            actual @?= expected
    ]
    
part1Solution :: Text -> Pressure
part1Solution t =
  findMaxPressure atlas initialRoute (Pressure 0)
 where
  initialRoute = [ Route
                      [(ValveID "AA", MinutesRemaining $ Minutes 30)]
                      (MinutesRemaining $ Minutes 30)
                      (OpenedValves S.empty)
                      (Pressure 0)
                  ]
  atlas = makeAtlas flows tunnels
  parser = fromRight [] . runParser (some (pLine <* optional newline)) ""
  inputLines = parser t
  flows = M.fromList $ map (\(InputLine v f _) -> (v, f)) inputLines
  tunnels = M.fromList $ map (\(InputLine v _ tv) -> (v, tv)) inputLines

makeTVS :: [Text] -> TunnelValves
makeTVS = TunnelValves . S.fromList . map ValveID

initialState :: State
initialState =
  State
    { minutesRemaining = MinutesRemaining (Minutes 30)
    , openedValves = OpenedValves S.empty
    , currentValve = ValveID "AA"
    }

env :: Env
env = Env flowMap tunnelMap simpleIndex uniformIndexSelector
 where
  flowMap =
    M.fromList
      [ (ValveID "AA", FlowRate 0)
      , (ValveID "BB", FlowRate 13)
      , (ValveID "CC", FlowRate 2)
      , (ValveID "DD", FlowRate 20)
      , (ValveID "EE", FlowRate 3)
      , (ValveID "FF", FlowRate 0)
      , (ValveID "GG", FlowRate 0)
      , (ValveID "HH", FlowRate 22)
      , (ValveID "II", FlowRate 0)
      , (ValveID "JJ", FlowRate 21)
      ]
  tunnelMap =
    M.fromList
      [ (ValveID "AA", makeTVS ["DD", "II", "BB"])
      , (ValveID "BB", makeTVS ["CC", "AA"])
      , (ValveID "CC", makeTVS ["DD", "BB"])
      , (ValveID "DD", makeTVS ["CC", "AA", "EE"])
      , (ValveID "EE", makeTVS ["FF", "DD"])
      , (ValveID "FF", makeTVS ["EE", "GG"])
      , (ValveID "GG", makeTVS ["FF", "HH"])
      , (ValveID "HH", makeTVS ["GG"])
      , (ValveID "II", makeTVS ["AA", "JJ"])
      , (ValveID "JJ", makeTVS ["II"])
      ]

exampleInput :: Text
exampleInput =
  T.intercalate
    "\n"
    [ "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
    , "Valve BB has flow rate=13; tunnels lead to valves CC, AA"
    , "Valve CC has flow rate=2; tunnels lead to valves DD, BB"
    , "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE"
    , "Valve EE has flow rate=3; tunnels lead to valves FF, DD"
    , "Valve FF has flow rate=0; tunnels lead to valves EE, GG"
    , "Valve GG has flow rate=0; tunnels lead to valves FF, HH"
    , "Valve HH has flow rate=22; tunnel leads to valve GG"
    , "Valve II has flow rate=0; tunnels lead to valves AA, JJ"
    , "Valve JJ has flow rate=21; tunnel leads to valve II"
    ]

puzzleInput :: Text
puzzleInput =
  T.intercalate
    "\n"
    [ "Valve AA has flow rate=0; tunnels lead to valves RZ, QQ, FH, IM, VJ"
    , "Valve FE has flow rate=0; tunnels lead to valves TM, TR"
    , "Valve QZ has flow rate=19; tunnels lead to valves HH, OY"
    , "Valve TU has flow rate=17; tunnels lead to valves NJ, IN, WN"
    , "Valve RG has flow rate=0; tunnels lead to valves IK, SZ"
    , "Valve TM has flow rate=0; tunnels lead to valves FE, JH"
    , "Valve JH has flow rate=4; tunnels lead to valves NW, QQ, TM, VH, AZ"
    , "Valve NW has flow rate=0; tunnels lead to valves JH, OB"
    , "Valve BZ has flow rate=0; tunnels lead to valves XG, XF"
    , "Valve VS has flow rate=0; tunnels lead to valves FF, GC"
    , "Valve OI has flow rate=20; tunnel leads to valve SY"
    , "Valve IK has flow rate=0; tunnels lead to valves RG, TR"
    , "Valve RO has flow rate=0; tunnels lead to valves UZ, YL"
    , "Valve LQ has flow rate=0; tunnels lead to valves IZ, PA"
    , "Valve GG has flow rate=18; tunnels lead to valves GH, VI"
    , "Valve NJ has flow rate=0; tunnels lead to valves TU, UZ"
    , "Valve SY has flow rate=0; tunnels lead to valves OI, ZL"
    , "Valve HH has flow rate=0; tunnels lead to valves QZ, WN"
    , "Valve RZ has flow rate=0; tunnels lead to valves AA, UZ"
    , "Valve OF has flow rate=0; tunnels lead to valves YL, IZ"
    , "Valve IZ has flow rate=9; tunnels lead to valves OF, FH, VH, JZ, LQ"
    , "Valve OB has flow rate=0; tunnels lead to valves UZ, NW"
    , "Valve AH has flow rate=0; tunnels lead to valves FF, ZL"
    , "Valve ZL has flow rate=11; tunnels lead to valves SY, VI, AH"
    , "Valve BF has flow rate=0; tunnels lead to valves PA, YL"
    , "Valve OH has flow rate=0; tunnels lead to valves CU, JZ"
    , "Valve VH has flow rate=0; tunnels lead to valves IZ, JH"
    , "Valve AZ has flow rate=0; tunnels lead to valves JC, JH"
    , "Valve XG has flow rate=0; tunnels lead to valves BZ, PA"
    , "Valve OY has flow rate=0; tunnels lead to valves PZ, QZ"
    , "Valve IM has flow rate=0; tunnels lead to valves FM, AA"
    , "Valve GO has flow rate=0; tunnels lead to valves VJ, TR"
    , "Valve YL has flow rate=8; tunnels lead to valves JC, RO, OF, BF, FM"
    , "Valve TY has flow rate=0; tunnels lead to valves SZ, TS"
    , "Valve UZ has flow rate=5; tunnels lead to valves OB, NJ, RO, RZ, GC"
    , "Valve XF has flow rate=21; tunnel leads to valve BZ"
    , "Valve RY has flow rate=0; tunnels lead to valves TR, FF"
    , "Valve QQ has flow rate=0; tunnels lead to valves JH, AA"
    , "Valve TS has flow rate=0; tunnels lead to valves TY, FF"
    , "Valve GC has flow rate=0; tunnels lead to valves VS, UZ"
    , "Valve JC has flow rate=0; tunnels lead to valves AZ, YL"
    , "Valve JZ has flow rate=0; tunnels lead to valves IZ, OH"
    , "Valve IN has flow rate=0; tunnels lead to valves TH, TU"
    , "Valve FM has flow rate=0; tunnels lead to valves IM, YL"
    , "Valve FH has flow rate=0; tunnels lead to valves AA, IZ"
    , "Valve VJ has flow rate=0; tunnels lead to valves AA, GO"
    , "Valve TH has flow rate=0; tunnels lead to valves CU, IN"
    , "Valve TR has flow rate=7; tunnels lead to valves FE, IK, RY, GO"
    , "Valve GH has flow rate=0; tunnels lead to valves GG, FF"
    , "Valve SZ has flow rate=10; tunnels lead to valves RG, TY"
    , "Valve PA has flow rate=16; tunnels lead to valves XG, LQ, BF"
    , "Valve PZ has flow rate=0; tunnels lead to valves CU, OY"
    , "Valve VI has flow rate=0; tunnels lead to valves ZL, GG"
    , "Valve CU has flow rate=22; tunnels lead to valves PZ, OH, TH"
    , "Valve WN has flow rate=0; tunnels lead to valves TU, HH"
    , "Valve FF has flow rate=13; tunnels lead to valves VS, RY, AH, TS, GH"
    ]
