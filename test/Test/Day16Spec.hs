module Test.Day16Spec (spec) where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Either (fromRight)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Day16
import System.Random
import Test.Hspec

spec :: SpecWith ()
spec =
  describe "Day 16 Tests" $ do
    it "Cumulative Sums" $ do
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
      cumsum (Pressure 0) ps `shouldBe` expected

    it "Minute Maps" $ do
      let expected =
            M.fromList
              [ (ValveID "AA", TravelMinutes $ Minutes 0)
              , (ValveID "BB", TravelMinutes $ Minutes 1)
              , (ValveID "CC", TravelMinutes $ Minutes 2)
              , (ValveID "DD", TravelMinutes $ Minutes 1)
              , (ValveID "EE", TravelMinutes $ Minutes 2)
              , (ValveID "FF", TravelMinutes $ Minutes 3)
              , (ValveID "GG", TravelMinutes $ Minutes 4)
              , (ValveID "HH", TravelMinutes $ Minutes 5)
              , (ValveID "II", TravelMinutes $ Minutes 1)
              , (ValveID "JJ", TravelMinutes $ Minutes 2)
              ]
          actual = travelMap (ValveID "AA") exampleTunnels
      actual `shouldBe` Right expected

    it "Pressure Maps" $ do
      let minutesRemaining = MinutesRemaining $ Minutes 30
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
          travelM = fromRight M.empty $ travelMap (ValveID "AA") exampleTunnels
          actual = pressureMap minutesRemaining flowMap travelM
          expected =
            M.fromList
              [ (ValveID "AA", (Pressure 0, MinutesRemaining $ Minutes 29))
              , (ValveID "BB", (Pressure 364, MinutesRemaining $ Minutes 28))
              , (ValveID "CC", (Pressure 54, MinutesRemaining $ Minutes 27))
              , (ValveID "DD", (Pressure 560, MinutesRemaining $ Minutes 28))
              , (ValveID "EE", (Pressure 81, MinutesRemaining $ Minutes 27))
              , (ValveID "FF", (Pressure 0, MinutesRemaining $ Minutes 26))
              , (ValveID "GG", (Pressure 0, MinutesRemaining $ Minutes 25))
              , (ValveID "HH", (Pressure 528, MinutesRemaining $ Minutes 24))
              , (ValveID "II", (Pressure 0, MinutesRemaining $ Minutes 28))
              , (ValveID "JJ", (Pressure 567, MinutesRemaining $ Minutes 27))
              ]
      actual `shouldBe` Right expected

    it "Pressure Tests" $ do
      let released =
            pressure
              (TravelMinutes (Minutes 1))
              (MinutesRemaining (Minutes 30))
              (FlowRate 13)
      released `shouldBe` (Pressure 364, MinutesRemaining (Minutes 28))

    it "Total Release" $ do
      let flowMap =
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
          actions =
            [ MoveToValve (ValveID "DD")
            , OpenValve (ValveID "DD")
            , MoveToValve (ValveID "CC")
            , MoveToValve (ValveID "BB")
            , OpenValve (ValveID "BB")
            , MoveToValve (ValveID "AA")
            , MoveToValve (ValveID "II")
            , MoveToValve (ValveID "JJ")
            , OpenValve (ValveID "JJ")
            , MoveToValve (ValveID "II")
            , MoveToValve (ValveID "AA")
            , MoveToValve (ValveID "DD")
            , MoveToValve (ValveID "EE")
            , MoveToValve (ValveID "FF")
            , MoveToValve (ValveID "GG")
            , MoveToValve (ValveID "HH")
            , OpenValve (ValveID "HH")
            , MoveToValve (ValveID "GG")
            , MoveToValve (ValveID "FF")
            , MoveToValve (ValveID "EE")
            , OpenValve (ValveID "EE")
            , MoveToValve (ValveID "DD")
            , MoveToValve (ValveID "CC")
            , OpenValve (ValveID "CC")
            , DoNothing
            , DoNothing
            , DoNothing
            , DoNothing
            , DoNothing
            , DoNothing
            ]
          ctx = Context flowMap tunnelMap
          expected =
            [ (initialState, FlowRate 0)
            , (State (ValveID "DD") (makeOVS []) (Minutes 1), FlowRate 0)
            , (State (ValveID "DD") (makeOVS ["DD"]) (Minutes 2), FlowRate 0)
            , (State (ValveID "CC") (makeOVS ["DD"]) (Minutes 3), FlowRate 20) -- delta=20, 20*27=540
            , (State (ValveID "BB") (makeOVS ["DD"]) (Minutes 4), FlowRate 20)
            , (State (ValveID "BB") (makeOVS ["DD", "BB"]) (Minutes 5), FlowRate 20)
            , (State (ValveID "AA") (makeOVS ["DD", "BB"]) (Minutes 6), FlowRate 33) -- delta=13, 13*23=299
            , (State (ValveID "II") (makeOVS ["DD", "BB"]) (Minutes 7), FlowRate 33)
            , (State (ValveID "JJ") (makeOVS ["DD", "BB"]) (Minutes 8), FlowRate 33)
            , (State (ValveID "JJ") (makeOVS ["DD", "BB", "JJ"]) (Minutes 9), FlowRate 33)
            , (State (ValveID "II") (makeOVS ["DD", "BB", "JJ"]) (Minutes 10), FlowRate 54) -- delta=21, 21*20=210
            , (State (ValveID "AA") (makeOVS ["DD", "BB", "JJ"]) (Minutes 11), FlowRate 54)
            , (State (ValveID "DD") (makeOVS ["DD", "BB", "JJ"]) (Minutes 12), FlowRate 54)
            , (State (ValveID "EE") (makeOVS ["DD", "BB", "JJ"]) (Minutes 13), FlowRate 54)
            , (State (ValveID "FF") (makeOVS ["DD", "BB", "JJ"]) (Minutes 14), FlowRate 54)
            , (State (ValveID "GG") (makeOVS ["DD", "BB", "JJ"]) (Minutes 15), FlowRate 54)
            , (State (ValveID "HH") (makeOVS ["DD", "BB", "JJ"]) (Minutes 16), FlowRate 54)
            , (State (ValveID "HH") (makeOVS ["DD", "BB", "JJ", "HH"]) (Minutes 17), FlowRate 54)
            , (State (ValveID "GG") (makeOVS ["DD", "BB", "JJ", "HH"]) (Minutes 18), FlowRate 76) -- delta=22, 22*12=264
            , (State (ValveID "FF") (makeOVS ["DD", "BB", "JJ", "HH"]) (Minutes 19), FlowRate 76)
            , (State (ValveID "EE") (makeOVS ["DD", "BB", "JJ", "HH"]) (Minutes 20), FlowRate 76)
            , (State (ValveID "EE") (makeOVS ["DD", "BB", "JJ", "HH", "EE"]) (Minutes 21), FlowRate 76)
            , (State (ValveID "DD") (makeOVS ["DD", "BB", "JJ", "HH", "EE"]) (Minutes 22), FlowRate 79) -- delta=3, 3*8=24
            , (State (ValveID "CC") (makeOVS ["DD", "BB", "JJ", "HH", "EE"]) (Minutes 23), FlowRate 79)
            , (State (ValveID "CC") (makeOVS ["DD", "BB", "JJ", "HH", "EE", "CC"]) (Minutes 24), FlowRate 79)
            , (State (ValveID "CC") (makeOVS ["DD", "BB", "JJ", "HH", "EE", "CC"]) (Minutes 25), FlowRate 81) -- delta=2, 2*5=10
            , (State (ValveID "CC") (makeOVS ["DD", "BB", "JJ", "HH", "EE", "CC"]) (Minutes 26), FlowRate 81)
            , (State (ValveID "CC") (makeOVS ["DD", "BB", "JJ", "HH", "EE", "CC"]) (Minutes 27), FlowRate 81)
            , (State (ValveID "CC") (makeOVS ["DD", "BB", "JJ", "HH", "EE", "CC"]) (Minutes 28), FlowRate 81)
            , (State (ValveID "CC") (makeOVS ["DD", "BB", "JJ", "HH", "EE", "CC"]) (Minutes 29), FlowRate 81)
            , (State (ValveID "CC") (makeOVS ["DD", "BB", "JJ", "HH", "EE", "CC"]) (Minutes 30), FlowRate 81)
            ]
          makeTVS = TunnelValves . S.fromList . map ValveID
          makeOVS = OpenedValves . S.fromList . map ValveID
          initialState = State (ValveID "AA") (OpenedValves S.empty) (Minutes 0)
      runActions ctx initialState actions `shouldBe` Right expected

    describe "Choosing Valves" $ do
      it "One positive valve" $ do
        let rand = mkStdGen 42
            opened = OpenedValves S.empty
            pressures =
              M.singleton
                (ValveID "A")
                (Pressure 1, MinutesRemaining $ Minutes 1)
            choice = chooseNextValve rand opened pressures
            indices =
              M.singleton
                (PressureIndex (Pressure 1))
                (ValveID "A")
        calculateRange pressures `shouldBe` PressureRange (Pressure 1)
        pressureIndex pressures `shouldBe` indices
        fst <$> choice `shouldBe` Just (ValveID "A")

      it "No positive valves" $ do
        let rand = mkStdGen 42
            opened = OpenedValves S.empty
            pressures =
              M.singleton
                (ValveID "A")
                (Pressure 0, MinutesRemaining $ Minutes 1)
            choice = chooseNextValve rand opened pressures
        choice `shouldBe` Nothing

      it "Only one positive valve and the rest zero" $ do
        let rand = mkStdGen 42
            opened = OpenedValves S.empty
            pressures =
              M.fromList
                [ (ValveID "A", (Pressure 0, MinutesRemaining $ Minutes 1))
                , (ValveID "B", (Pressure 0, MinutesRemaining $ Minutes 1))
                , (ValveID "C", (Pressure 1, MinutesRemaining $ Minutes 1))
                , (ValveID "D", (Pressure 0, MinutesRemaining $ Minutes 1))
                , (ValveID "E", (Pressure 0, MinutesRemaining $ Minutes 1))
                , (ValveID "F", (Pressure 0, MinutesRemaining $ Minutes 1))
                ]
            choice = chooseNextValve rand opened pressures
        fst <$> choice `shouldBe` Just (ValveID "C")

      it "Only choosing positive valves" $ do
        let opened = OpenedValves S.empty
            pressures =
              M.fromList
                [ (ValveID "A", (Pressure 0, MinutesRemaining $ Minutes 1))
                , (ValveID "B", (Pressure 0, MinutesRemaining $ Minutes 1))
                , (ValveID "C", (Pressure 10, MinutesRemaining $ Minutes 1))
                , (ValveID "D", (Pressure 0, MinutesRemaining $ Minutes 1))
                , (ValveID "E", (Pressure 10, MinutesRemaining $ Minutes 1))
                , (ValveID "F", (Pressure 3, MinutesRemaining $ Minutes 1))
                ]

            choices = sequence $ flip evalState (mkStdGen 42) $ do
              forM [1 :: Int .. 100] $ \_ -> do
                g <- get
                case chooseNextValve g opened pressures of
                  Nothing -> return Nothing
                  Just (valve, g') -> do
                    put g'
                    return (Just valve)
            expectedValves = S.fromList $ map ValveID ["C", "E", "F"]
            actual = S.fromList <$> choices
        actual `shouldBe` Just expectedValves

      it "Only choosing positive valves in proportional ratios" $ do
        let opened = OpenedValves S.empty
            pressures =
              M.fromList
                [ (ValveID "A", (Pressure 0, MinutesRemaining $ Minutes 1))
                , (ValveID "B", (Pressure 0, MinutesRemaining $ Minutes 1))
                , (ValveID "C", (Pressure 100, MinutesRemaining $ Minutes 1))
                , (ValveID "D", (Pressure 0, MinutesRemaining $ Minutes 1))
                , (ValveID "E", (Pressure 0, MinutesRemaining $ Minutes 1))
                , (ValveID "F", (Pressure 1, MinutesRemaining $ Minutes 1))
                ]
            choices = sequence $ flip evalState (mkStdGen 42) $ do
              forM [1 :: Int .. 10000] $ \_ -> do
                g <- get
                case chooseNextValve g opened pressures of
                  Nothing -> return Nothing
                  Just (valve, g') -> do
                    put g'
                    return (Just valve)
            numChoiceC = length . filter (== ValveID "C") $ fromJust choices
            numChoiceF = length . filter (== ValveID "F") $ fromJust choices
        numChoiceF `shouldBe` 94
        numChoiceC `shouldBe` 9906

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

exampleTunnels :: TunnelMap
exampleTunnels =
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
 where
  makeTVS = TunnelValves . S.fromList . map ValveID