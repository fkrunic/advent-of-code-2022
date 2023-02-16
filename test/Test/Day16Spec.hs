module Test.Day16Spec (spec) where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Day16
import Test.Hspec

spec :: SpecWith ()
spec =
  describe "Day 16 Tests" $ do
    it "Minute Maps" $ do
      let expected =
            M.fromList $
              [ (ValveID "AA", Minutes 0)
              , (ValveID "BB", Minutes 1)
              , (ValveID "CC", Minutes 2)
              , (ValveID "DD", Minutes 1)
              , (ValveID "EE", Minutes 2)
              , (ValveID "FF", Minutes 3)
              , (ValveID "GG", Minutes 4)
              , (ValveID "HH", Minutes 5)
              , (ValveID "II", Minutes 1)
              , (ValveID "JJ", Minutes 2)
              ]
          actual = minuteMap (ValveID "AA") exampleTunnels
      actual `shouldBe` Right expected

    it "Pressure Tests" $ do
      let released =
            pressure
              (TravelMinutes (Minutes 1))
              (MinutesRemaining (Minutes 30))
              (FlowRate 13)
      released `shouldBe` Just (Pressure 364, MinutesRemaining (Minutes 28))

    it "Total Release" $ do
      let actions = 
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


exampleInput :: Text
exampleInput =
  T.intercalate
    "\n"
    [ "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
    , "Valve BB has flow rate=13; tunnels lead to valves CC, AA"
    , "Valve CC has flow rate=2; tunnels lead to valves DD, BB"
    , "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE"
    , "Valve EE has flow rate=3; tunnels lead to valves FF, DD"
    , -- , "Valve FF has flow rate=0; tunnels lead to valves EE, GG"
      -- , "Valve GG has flow rate=0; tunnels lead to valves FF, HH"
      "Valve HH has flow rate=22; tunnel leads to valve GG"
    , -- , "Valve II has flow rate=0; tunnels lead to valves AA, JJ"
      "Valve JJ has flow rate=21; tunnel leads to valve II"
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