module Test.Day15Spec (spec) where

import Test.Hspec

import Data.Either (fromRight)
import Data.Map qualified as M
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Problems.Day15

import Utilities.Grids

import Text.Megaparsec

spec :: SpecWith ()
spec =
  describe "Day 15 Tests" $ do
    describe "Parsing Tests" $ do
      it "Single Beacon Line" $ do
        let line = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
            actual = runParser pLine "" line
            expected = sensorPair $ head exampleSpread
        actual `shouldBe` Right expected

      it "Example Input" $ do
        parser exampleInput `shouldBe` exampleSpread

    describe "Rendering Sensors and Beacons" $ do
      it "Can render a single beacon" $ do
        let sp =
              SensorPair
                { sensorID = SensorID 1
                , sensorPair =
                    (SensorLocation (point 0 0), BeaconLocation (point 2 0))
                }
            locs = [sp]
            actual = renderField (LocationLayout locs Nothing)
            expected =
              T.intercalate
                "\n"
                [ "..#.."
                , ".###."
                , "##S#B"
                , ".###."
                , "..#.."
                ]
        actual `shouldBe` expected

      it "Can render two separated beacons" $ do
        let sp1 =
              SensorPair
                { sensorID = SensorID 1
                , sensorPair =
                    (SensorLocation (point 0 0), BeaconLocation (point 2 0))
                }
            sp2 =
              SensorPair
                { sensorID = SensorID 2
                , sensorPair =
                    (SensorLocation (point 7 0), BeaconLocation (point 6 0))
                }
            locs = [sp1, sp2]
            actual = renderField (LocationLayout locs Nothing)
            expected =
              T.intercalate
                "\n"
                [ "..#........"
                , ".###.....#."
                , "##S#B...BS#"
                , ".###.....#."
                , "..#........"
                ]
        actual `shouldBe` expected

      it "Can render two overlapping sensors" $ do
        let sp1 =
              SensorPair
                { sensorID = SensorID 1
                , sensorPair = (SensorLocation (point 0 0), BeaconLocation (point 5 0))
                }
            sp2 =
              SensorPair
                { sensorID = SensorID 2
                , sensorPair = (SensorLocation (point 3 (-3)), BeaconLocation (point 5 (-3)))
                }
            locs = [sp1, sp2]
            actual = renderField (LocationLayout locs Nothing)
            expected =
              T.intercalate
                "\n"
                [ ".....#..#.."
                , "....######."
                , "...#####S#B"
                , "..########."
                , ".#########."
                , "#####S####B"
                , ".#########."
                , "..#######.."
                , "...#####..."
                , "....###...."
                , ".....#....."
                ]
        actual `shouldBe` expected

      it "Can render the example grid" $ do
        let layout = LocationLayout exampleSpread Nothing
            actual = renderField layout
            expected =
              T.intercalate
                "\n"
                [ "..........#.........................."
                , ".........###........................."
                , "........#####........................"
                , ".......#######......................."
                , "......#########.............#........"
                , ".....###########...........###......."
                , "....#############.........#####......"
                , "...###############.......#######....."
                , "..#################.....#########...."
                , ".###################.#.###########..."
                , "##########S########################.."
                , ".###########################S#######."
                , "..###################S#############.."
                , "...###################SB##########..."
                , "....#############################...."
                , ".....###########################....."
                , "......#########################......"
                , ".......#########S#######S#####......."
                , "........#######################......"
                , ".......#########################....."
                , "......####B######################...."
                , ".....###S#############.###########..."
                , "......#############################.."
                , ".......#############################."
                , ".......#############S#######S########"
                , "......B#############################."
                , ".....############SB################.."
                , "....##################S##########B..."
                , "...#######S######################...."
                , "....############################....."
                , ".....#############S######S######....."
                , "......#########################......"
                , ".......#######..#############B......."
                , "........#####....###..#######........"
                , ".........###......#....#####........."
                , "..........#.............###.........."
                , ".........................#..........."
                ]
        actual `shouldBe` expected

      it "Can render an adjusted grid" $ do
        let actual = renderField (LocationLayout adjustedSpread Nothing)
            expected =
              T.intercalate
                "\n"
                [ "..........#.........................."
                , ".........###........................."
                , "........#####........................"
                , ".......#######......................."
                , "......#########.............#........"
                , ".....###########...........###......."
                , "....#############.........#####......"
                , "...###############.......#######....."
                , "..#################.....#########...."
                , ".###################...###########..."
                , "##########S##########.#############.."
                , ".###################.#######S#######."
                , "..#################...#############.."
                , "...###############.....B##########..."
                , "....#############.......#########...."
                , ".....###########.........#######....."
                , "......#########...........#####......"
                , ".......#######.............###......."
                , "........#####.............#####......"
                , ".........###.............#######....."
                , "..........B.............#########...."
                , "..........#............###########..."
                , ".........###..........#############.."
                , "........#####........###############."
                , ".......#######......########S########"
                , "......B########....#################."
                , ".....###########..B################.."
                , "....##################S##########B..."
                , "...#######S######################...."
                , "....#############..#############....."
                , ".....###########...######S######....."
                , "......#########.....###########......"
                , ".......#######.......########B......."
                , "........#####.........#######........"
                , ".........###...........#####........."
                , "..........#.............###.........."
                , ".........................#..........."
                ]
        actual `shouldBe` expected

    describe "Boundary Teleportations" $ do
      it "A marker outside sensor range is not teleported" $ do
        let sLoc = point 0 0
            bLoc = point 2 2
            mkLoc = point (-5) 0
            pair = (SensorLocation sLoc, BeaconLocation bLoc)
            scanner = makeScanner pair
            sPair = SensorPair (SensorID 1) pair
            layout = LocationLayout [sPair] (Just $ MarkerLocation mkLoc)
            expectedRender =
              T.intercalate
                "\n"
                [ ".....#...."
                , "....###..."
                , "...#####.."
                , "..#######."
                , "X####S####"
                , "..#######."
                , "...####B.."
                , "....###..."
                , ".....#...."
                ]
        renderField layout `shouldBe` expectedRender
        teleportAcrossSensor mkLoc (fst pair) scanner `shouldBe` Nothing

      it "A marker strictly contained in quadrant is teleported correctly" $ do
        let sLoc = point 0 0
            bLoc = point 2 2
            mkLoc = point (-1) (-1)
            pair = (SensorLocation sLoc, BeaconLocation bLoc)
            scanner = makeScanner pair
            sPair = SensorPair (SensorID 1) pair
            layout = LocationLayout [sPair] (Just $ MarkerLocation mkLoc)
            expectedRender =
              T.intercalate
                "\n"
                [ "....#...."
                , "...###..."
                , "..#####.."
                , ".##X####."
                , "####S####"
                , ".#######."
                , "..####B.."
                , "...###..."
                , "....#...."
                ]
            teleportedMarker = teleportAcrossSensor mkLoc (fst pair) scanner
            teleportedLayout =
              LocationLayout [sPair] (MarkerLocation <$> teleportedMarker)
            expectedTeleport =
              T.intercalate
                "\n"
                [ "....#...."
                , "...###..."
                , "..#####.."
                , ".######X."
                , "####S####"
                , ".#######."
                , "..####B.."
                , "...###..."
                , "....#...."
                ]

        deduceCurrentQuadrant mkLoc (SensorLocation sLoc) scanner `shouldBe` Just Q3
        renderField layout `shouldBe` expectedRender
        renderField teleportedLayout `shouldBe` expectedTeleport

      it "A marker on the north boundary stays the same after teleportation" $ do
        let sLoc = point 0 0
            bLoc = point 2 2
            mkLoc = point 0 (-4)
            pair = (SensorLocation sLoc, BeaconLocation bLoc)
            scanner = makeScanner pair
            sPair = SensorPair (SensorID 1) pair
            layout = LocationLayout [sPair] (Just $ MarkerLocation mkLoc)
            expectedRender =
              T.intercalate
                "\n"
                [ "....X...."
                , "...###..."
                , "..#####.."
                , ".#######."
                , "####S####"
                , ".#######."
                , "..####B.."
                , "...###..."
                , "....#...."
                ]
            teleportedMarker = teleportAcrossSensor mkLoc (fst pair) scanner
            teleportedLayout =
              LocationLayout [sPair] (MarkerLocation <$> teleportedMarker)
        renderField layout `shouldBe` expectedRender
        renderField teleportedLayout `shouldBe` expectedRender

      it "A marker on the south boundary stays the same after teleportation" $ do
        let sLoc = point 0 0
            bLoc = point 2 2
            mkLoc = point 0 4
            pair = (SensorLocation sLoc, BeaconLocation bLoc)
            scanner = makeScanner pair
            sPair = SensorPair (SensorID 1) pair
            layout = LocationLayout [sPair] (Just $ MarkerLocation mkLoc)
            expectedRender =
              T.intercalate
                "\n"
                [ "....#...."
                , "...###..."
                , "..#####.."
                , ".#######."
                , "####S####"
                , ".#######."
                , "..####B.."
                , "...###..."
                , "....X...."
                ]
            teleportedMarker = teleportAcrossSensor mkLoc (fst pair) scanner
            teleportedLayout =
              LocationLayout [sPair] (MarkerLocation <$> teleportedMarker)
        renderField layout `shouldBe` expectedRender
        renderField teleportedLayout `shouldBe` expectedRender  

      it "A marker on the east boundary stays the same after teleportation" $ do 
        let sLoc = point 0 0
            bLoc = point 2 2
            mkLoc = point 4 0
            pair = (SensorLocation sLoc, BeaconLocation bLoc)
            scanner = makeScanner pair
            sPair = SensorPair (SensorID 1) pair
            layout = LocationLayout [sPair] (Just $ MarkerLocation mkLoc)
            expectedRender =
              T.intercalate
                "\n"
                [ "....#...."
                , "...###..."
                , "..#####.."
                , ".#######."
                , "####S###X"
                , ".#######."
                , "..####B.."
                , "...###..."
                , "....#...."
                ]
            teleportedMarker = teleportAcrossSensor mkLoc (fst pair) scanner
            teleportedLayout =
              LocationLayout [sPair] (MarkerLocation <$> teleportedMarker)
        renderField layout `shouldBe` expectedRender
        renderField teleportedLayout `shouldBe` expectedRender  

      it "A marker on the west boundary is teleported to east boundary" $ do
        let sLoc = point 0 0
            bLoc = point 2 2
            mkLoc = point (-4) 0
            pair = (SensorLocation sLoc, BeaconLocation bLoc)
            scanner = makeScanner pair
            sPair = SensorPair (SensorID 1) pair
            layout = LocationLayout [sPair] (Just $ MarkerLocation mkLoc)
            expectedRender =
              T.intercalate
                "\n"
                [ "....#...."
                , "...###..."
                , "..#####.."
                , ".#######."
                , "X###S####"
                , ".#######."
                , "..####B.."
                , "...###..."
                , "....#...."
                ]
            teleportedMarker = teleportAcrossSensor mkLoc (fst pair) scanner
            teleportedLayout =
              LocationLayout [sPair] (MarkerLocation <$> teleportedMarker)
            expectedTeleport =
              T.intercalate
                "\n"
                [ "....#...."
                , "...###..."
                , "..#####.."
                , ".#######."
                , "####S###X"
                , ".#######."
                , "..####B.."
                , "...###..."
                , "....#...."
                ]

        renderField layout `shouldBe` expectedRender
        renderField teleportedLayout `shouldBe` expectedTeleport    

      it "A marker in a cross-quadrant is just pushed to the boundary" $ do
        let sLoc = point 0 0
            bLoc = point 2 2
            mkLoc = point 2 1
            pair = (SensorLocation sLoc, BeaconLocation bLoc)
            scanner = makeScanner pair
            sPair = SensorPair (SensorID 1) pair
            layout = LocationLayout [sPair] (Just $ MarkerLocation mkLoc)
            expectedRender =
              T.intercalate
                "\n"
                [ "....#...."
                , "...###..."
                , "..#####.."
                , ".#######."
                , "####S####"
                , ".#####X#."
                , "..####B.."
                , "...###..."
                , "....#...."
                ]
            teleportedMarker = teleportAcrossSensor mkLoc (fst pair) scanner
            teleportedLayout =
              LocationLayout [sPair] (MarkerLocation <$> teleportedMarker)
            expectedTeleport =
              T.intercalate
                "\n"
                [ "....#...."
                , "...###..."
                , "..#####.."
                , ".#######."
                , "####S####"
                , ".######X."
                , "..####B.."
                , "...###..."
                , "....#...."
                ]
        renderField layout `shouldBe` expectedRender
        renderField teleportedLayout `shouldBe` expectedTeleport                         

    describe "Puzzle Solutions" $ do
      it "Part 1 - Example Input" $ do
        part1Solution (YCoordinate 10) exampleInput `shouldBe` 26

      it "Part 1 - Puzzle Input" $ do
        pendingWith "solving part 2"

      it "Part 2 - Example Solution" $ do
        let bounds = 
              Boundaries 
                (XCoordinate 0) 
                (XCoordinate 20) 
                (YCoordinate 0) 
                (YCoordinate 20)
        distress exampleSpread bounds `shouldBe` Right (point 14 11)

      it "Part 2 - Puzzle Input" $ do
        let bounds = 
              Boundaries 
                (XCoordinate 0) 
                (XCoordinate 4000000) 
                (YCoordinate 0) 
                (YCoordinate 4000000)
            distressCoord = distress (parser puzzleInput) bounds
            freq = tuningFrequency <$> distressCoord
        distressCoord `shouldBe` Right (point 3129625 2636475)
        freq `shouldBe` Right 12518502636475

-- part1Solution (YCoordinate 2000000) puzzleInput `shouldBe` 0

part1Solution :: YCoordinate -> Text -> Int
part1Solution rowY t = length $ filter (== Empty) tiles
 where
  sps = parser t
  scanner =
    activateAll $
      map (\sp -> (sensorID sp, makeScanner (sensorPair sp))) sps
  grid = generateGrid (LocationLayout sps Nothing)
  Boundaries xMin xMax _ _ = getBounds $ M.keys grid
  tiles =
    map (\x -> determineCell (isJust . scanner) (x, rowY) grid) [xMin .. xMax]

parser :: Text -> [SensorPair]
parser = fromRight [] . runParser pSensors ""

exampleInput :: Text
exampleInput =
  T.intercalate
    "\n"
    [ "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
    , "Sensor at x=9, y=16: closest beacon is at x=10, y=16"
    , "Sensor at x=13, y=2: closest beacon is at x=15, y=3"
    , "Sensor at x=12, y=14: closest beacon is at x=10, y=16"
    , "Sensor at x=10, y=20: closest beacon is at x=10, y=16"
    , "Sensor at x=14, y=17: closest beacon is at x=10, y=16"
    , "Sensor at x=8, y=7: closest beacon is at x=2, y=10"
    , "Sensor at x=2, y=0: closest beacon is at x=2, y=10"
    , "Sensor at x=0, y=11: closest beacon is at x=2, y=10"
    , "Sensor at x=20, y=14: closest beacon is at x=25, y=17"
    , "Sensor at x=17, y=20: closest beacon is at x=21, y=22"
    , "Sensor at x=16, y=7: closest beacon is at x=15, y=3"
    , "Sensor at x=14, y=3: closest beacon is at x=15, y=3"
    , "Sensor at x=20, y=1: closest beacon is at x=15, y=3"
    ]

exampleSpread :: [SensorPair]
exampleSpread =
  [ SensorPair (SensorID 1) (SensorLocation $ point 2 18, BeaconLocation $ point (-2) 15)
  , SensorPair (SensorID 2) (SensorLocation $ point 9 16, BeaconLocation $ point 10 16)
  , SensorPair (SensorID 3) (SensorLocation $ point 13 2, BeaconLocation $ point 15 3)
  , SensorPair (SensorID 4) (SensorLocation $ point 12 14, BeaconLocation $ point 10 16)
  , SensorPair (SensorID 5) (SensorLocation $ point 10 20, BeaconLocation $ point 10 16)
  , SensorPair (SensorID 6) (SensorLocation $ point 14 17, BeaconLocation $ point 10 16)
  , SensorPair (SensorID 7) (SensorLocation $ point 8 7, BeaconLocation $ point 2 10)
  , SensorPair (SensorID 8) (SensorLocation $ point 2 0, BeaconLocation $ point 2 10)
  , SensorPair (SensorID 9) (SensorLocation $ point 0 11, BeaconLocation $ point 2 10)
  , SensorPair (SensorID 10) (SensorLocation $ point 20 14, BeaconLocation $ point 25 17)
  , SensorPair (SensorID 11) (SensorLocation $ point 17 20, BeaconLocation $ point 21 22)
  , SensorPair (SensorID 12) (SensorLocation $ point 16 7, BeaconLocation $ point 15 3)
  , SensorPair (SensorID 13) (SensorLocation $ point 14 3, BeaconLocation $ point 15 3)
  , SensorPair (SensorID 14) (SensorLocation $ point 20 1, BeaconLocation $ point 15 3)
  ]

-- 2,3,4,5,7,9,12,13
adjustedSpread :: [SensorPair]
adjustedSpread =
  [ SensorPair (SensorID 1) (SensorLocation $ point 2 18, BeaconLocation $ point (-2) 15)
  , -- , SensorPair (SensorID 2) (SensorLocation $ point 9 16, BeaconLocation $ point 10 16)
    -- , SensorPair (SensorID 3) (SensorLocation $ point 13 2, BeaconLocation $ point 15 3)
    -- , SensorPair (SensorID 4) (SensorLocation $ point 12 14, BeaconLocation $ point 10 16)
    -- , SensorPair (SensorID 5) (SensorLocation $ point 10 20, BeaconLocation $ point 10 16)
    SensorPair (SensorID 6) (SensorLocation $ point 14 17, BeaconLocation $ point 10 16)
  , -- , SensorPair (SensorID 7) (SensorLocation $ point 8 7, BeaconLocation $ point 2 10)
    SensorPair (SensorID 8) (SensorLocation $ point 2 0, BeaconLocation $ point 2 10)
  , -- , SensorPair (SensorID 9) (SensorLocation $ point 0 11, BeaconLocation $ point 2 10)
    SensorPair (SensorID 10) (SensorLocation $ point 20 14, BeaconLocation $ point 25 17)
  , SensorPair (SensorID 11) (SensorLocation $ point 17 20, BeaconLocation $ point 21 22)
  , -- , SensorPair (SensorID 12) (SensorLocation $ point 16 7, BeaconLocation $ point 15 3)
    -- , SensorPair (SensorID 13) (SensorLocation $ point 14 3, BeaconLocation $ point 15 3)
    SensorPair (SensorID 14) (SensorLocation $ point 20 1, BeaconLocation $ point 15 3)
  ]

puzzleInput :: Text
puzzleInput =
  T.intercalate
    "\n"
    [ "Sensor at x=2765643, y=3042538: closest beacon is at x=2474133, y=3521072"
    , "Sensor at x=2745662, y=2324735: closest beacon is at x=2491341, y=1883354"
    , "Sensor at x=2015742, y=2904055: closest beacon is at x=2474133, y=3521072"
    , "Sensor at x=3375262, y=3203288: closest beacon is at x=3321219, y=3415236"
    , "Sensor at x=3276468, y=3892409: closest beacon is at x=3321219, y=3415236"
    , "Sensor at x=952573, y=3147055: closest beacon is at x=-41010, y=2905006"
    , "Sensor at x=1823659, y=1779343: closest beacon is at x=1592718, y=2000000"
    , "Sensor at x=1156328, y=865741: closest beacon is at x=1592718, y=2000000"
    , "Sensor at x=3938443, y=271482: closest beacon is at x=4081274, y=1177185"
    , "Sensor at x=2815232, y=1641178: closest beacon is at x=2491341, y=1883354"
    , "Sensor at x=3984799, y=3424711: closest beacon is at x=3321219, y=3415236"
    , "Sensor at x=1658825, y=3999931: closest beacon is at x=2474133, y=3521072"
    , "Sensor at x=3199859, y=1285962: closest beacon is at x=4081274, y=1177185"
    , "Sensor at x=3538649, y=2788193: closest beacon is at x=3725736, y=2414539"
    , "Sensor at x=3522208, y=3336284: closest beacon is at x=3321219, y=3415236"
    , "Sensor at x=3093758, y=3492396: closest beacon is at x=3321219, y=3415236"
    , "Sensor at x=2464979, y=562119: closest beacon is at x=2491341, y=1883354"
    , "Sensor at x=3665010, y=1556840: closest beacon is at x=3735739, y=2128164"
    , "Sensor at x=207525, y=3893957: closest beacon is at x=-41010, y=2905006"
    , "Sensor at x=3894678, y=1974599: closest beacon is at x=3735739, y=2128164"
    , "Sensor at x=2185146, y=3822275: closest beacon is at x=2474133, y=3521072"
    , "Sensor at x=31166, y=1467978: closest beacon is at x=-41010, y=2905006"
    , "Sensor at x=3242364, y=3335961: closest beacon is at x=3321219, y=3415236"
    , "Sensor at x=3773718, y=3999789: closest beacon is at x=3321219, y=3415236"
    , "Sensor at x=423046, y=2227938: closest beacon is at x=-41010, y=2905006"
    , "Sensor at x=1600225, y=2529059: closest beacon is at x=1592718, y=2000000"
    , "Sensor at x=3291752, y=2241389: closest beacon is at x=3735739, y=2128164"
    , "Sensor at x=2741333, y=3984346: closest beacon is at x=2474133, y=3521072"
    , "Sensor at x=3935288, y=2292902: closest beacon is at x=3725736, y=2414539"
    , "Sensor at x=291635, y=140996: closest beacon is at x=212146, y=-1154950"
    , "Sensor at x=3966296, y=2600346: closest beacon is at x=3725736, y=2414539"
    , "Sensor at x=2228916, y=1461096: closest beacon is at x=2491341, y=1883354"
    ]
