module Test.Day17Spec where

import Data.List.NonEmpty as NE
import Data.Text qualified as T
import Problems.Day17
import Test.Input
import Utilities.Grids
import Utilities.Infinites

import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec = do
  testGroup "Day 17 Tests" $
    [ miscTests
    , caveTests
    , rockProcessTests
    , shapeTests
    , startingPositionTests
    , towerProcessTests
    , heightTests
    , solutionTests
    ]

miscTests :: TestTree
miscTests = 
  testGroup "Miscellaneous Tests" $
    [ testCase "Parsing wind direction" $
        let actual = parse ">>><<"
            expected = [East, East, East, West, West]
        in actual @?= expected
    ]

caveTests :: TestTree
caveTests = 
  let input = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
      winds = case NE.nonEmpty (parse input) of
        Nothing -> error "Cannot parse wind input"
        Just wds -> makeInf wds
      rts = makeInf $ HLine :| [Plus .. Square]
  in testGroup "Cave Tests" $
      [ testCase "Can draw the initial cave" $
          let actual = drawCave caveFloor
              expected = "#######"
          in actual @?= expected
      ]
      
rockProcessTests :: TestTree
rockProcessTests = 
  let input = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
      winds = case NE.nonEmpty (parse input) of
        Nothing -> error "Cannot parse wind input"
        Just wds -> makeInf wds
      rts = makeInf $ HLine :| [Plus .. Square]

  in testGroup "Rock Process" $
    [ testCase "Rock 1" $
        let rp = startingPos caveFloor HLine
            (settled, _) = rockProcess caveFloor rp winds
            actual = drawCave settled
            expected =
              T.intercalate
                "\n"
                [ "..####."
                , "#######"
                ]
        in actual @?= expected

    , testCase "Rock 2" $
          let rp1 = startingPos caveFloor HLine
              (c1, futureWinds) = rockProcess caveFloor rp1 winds
              (nextWind, _) = getSplit futureWinds
              rp2 = startingPos c1 Plus
              (settled, _) = rockProcess c1 rp2 futureWinds
              actual = drawCave settled
              expected =
                T.intercalate
                  "\n"
                  [ "...#..."
                  , "..###.."
                  , "...#..."
                  , "..####."
                  , "#######"
                  ]
          in do 
            nextWind @?= West
            actual @?= expected
    ]

shapeTests :: TestTree
shapeTests = 
  testGroup "Shapes" $
    [ testCase "Shifting anchor of square" $
        let square = typeShape Square
            desiredAnchor = point 5 5
            actual = square `shiftAnchorTo` desiredAnchor
            expected =
              InternalAnchor $
                InternallyAnchoredShape
                  { anchor = point 5 5
                  , remainder = [point 5 4, point 6 4, point 6 5]
                  , bottom = point 5 5 :| [point 6 5]
                  }
        in actual @?= expected
    ]

startingPositionTests :: TestTree
startingPositionTests = 
  let input = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
      winds = case NE.nonEmpty (parse input) of
        Nothing -> error "Cannot parse wind input"
        Just wds -> makeInf wds
      rts = makeInf $ HLine :| [Plus .. Square]

  in testGroup "Starting Positions" $
    [ testCase "Starting Position - Rock 1" $
          let rp = startingPos caveFloor HLine
              settled = settleRock caveFloor rp
              actual = drawCave settled
              expected =
                T.intercalate
                  "\n"
                  [ "..####."
                  , "......."
                  , "......."
                  , "......."
                  , "#######"
                  ]
          in actual @?= expected

  , testCase "Starting Position - Rock 2" $
          let c1 = towerProcess caveFloor rts winds 1
              rp = startingPos c1 Plus
              settled = settleRock c1 rp
              actual = drawCave settled
              expected =
                T.intercalate
                  "\n"
                  [ "...#..."
                  , "..###.."
                  , "...#..."
                  , "......."
                  , "......."
                  , "......."
                  , "..####."
                  , "#######"
                  ]
          in actual @?= expected
    ]

towerProcessTests :: TestTree
towerProcessTests = 
  let input = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
      winds = case NE.nonEmpty (parse input) of
        Nothing -> error "Cannot parse wind input"
        Just wds -> makeInf wds
      rts = makeInf $ HLine :| [Plus .. Square]
      process = towerProcess caveFloor rts winds

  in testGroup "Tower Process" $
    [ testCase "Rock 1" $
        let actual = drawCave $ process 1
            expected =
              T.intercalate
                "\n"
                [ "..####."
                , "#######"
                ]
        in actual @?= expected

    , testCase "Rock 2" $
        let actual = drawCave $ process 2
            expected =
              T.intercalate
                "\n"
                [ "...#..."
                , "..###.."
                , "...#..."
                , "..####."
                , "#######"
                ]
        in actual @?= expected

    , testCase "Rock 3" $
        let actual = drawCave $ process 3
            expected =
              T.intercalate
                "\n"
                [ "..#...."
                , "..#...."
                , "####..."
                , "..###.."
                , "...#..."
                , "..####."
                , "#######"
                ]
        in actual @?= expected

    , testCase "Rock 4" $
        let actual = drawCave $ process 4
            expected =
              T.intercalate
                "\n"
                [ "....#.."
                , "..#.#.."
                , "..#.#.."
                , "#####.."
                , "..###.."
                , "...#..."
                , "..####."
                , "#######"
                ]
        in actual @?= expected

    , testCase "Rock 5" $
        let actual = drawCave $ process 5
            expected =
              T.intercalate
                "\n"
                [ "....##."
                , "....##."
                , "....#.."
                , "..#.#.."
                , "..#.#.."
                , "#####.."
                , "..###.."
                , "...#..."
                , "..####."
                , "#######"
                ]
        in actual @?= expected

  , testCase "Rock 6" $
      let actual = drawCave $ process 6
          expected =
            T.intercalate
              "\n"
              [ ".####.."
              , "....##."
              , "....##."
              , "....#.."
              , "..#.#.."
              , "..#.#.."
              , "#####.."
              , "..###.."
              , "...#..."
              , "..####."
              , "#######"
              ]
      in actual @?= expected

  , testCase "Rock 7" $
      let actual = drawCave $ process 7
          expected =
            T.intercalate
              "\n"
              [ "..#...."
              , ".###..."
              , "..#...."
              , ".####.."
              , "....##."
              , "....##."
              , "....#.."
              , "..#.#.."
              , "..#.#.."
              , "#####.."
              , "..###.."
              , "...#..."
              , "..####."
              , "#######"
              ]
      in actual @?= expected

  , testCase "Rock 8" $
      let actual = drawCave $ process 8
          expected =
            T.intercalate
              "\n"
              [ ".....#."
              , ".....#."
              , "..####."
              , ".###..."
              , "..#...."
              , ".####.."
              , "....##."
              , "....##."
              , "....#.."
              , "..#.#.."
              , "..#.#.."
              , "#####.."
              , "..###.."
              , "...#..."
              , "..####."
              , "#######"
              ]
      in actual @?= expected

  , testCase "Rock 9" $
      let actual = drawCave $ process 9
          expected =
                T.intercalate
                  "\n"
                  [ "....#.."
                  , "....#.."
                  , "....##."
                  , "....##."
                  , "..####."
                  , ".###..."
                  , "..#...."
                  , ".####.."
                  , "....##."
                  , "....##."
                  , "....#.."
                  , "..#.#.."
                  , "..#.#.."
                  , "#####.."
                  , "..###.."
                  , "...#..."
                  , "..####."
                  , "#######"
                  ]
      in actual @?= expected

  , testCase "Rock 10" $
      let actual = drawCave $ process 10
          expected =
            T.intercalate
              "\n"
              [ "....#.."
              , "....#.."
              , "....##."
              , "##..##."
              , "######."
              , ".###..."
              , "..#...."
              , ".####.."
              , "....##."
              , "....##."
              , "....#.."
              , "..#.#.."
              , "..#.#.."
              , "#####.."
              , "..###.."
              , "...#..."
              , "..####."
              , "#######"
              ]
      in actual @?= expected
    ]

heightTests :: TestTree
heightTests = 
  testGroup "Efficient Height" $
    [ multipleWindsTests
    , oneWindTests
    ]

multipleWindsTests :: TestTree
multipleWindsTests = 
  let windSet = East :| [East, West]
      rtsSet = HLine :| [VLine]

  in testGroup "Multiple Winds, Multiple Shapes (LCM=3x2=6)" $
        [ testCase "Iteration 0" $
            let heightActual = efficientHeight caveFloor rtsSet windSet 0
                processActual = towerProcess caveFloor (makeInf rtsSet) (makeInf windSet) 0
                caveActual = drawCave processActual
            in do 
              heightActual @?= 0
              caveActual @?= "#######"

        , testCase "Iteration 1" $
            let heightActual = efficientHeight caveFloor rtsSet windSet 1
                processActual = towerProcess caveFloor (makeInf rtsSet) (makeInf windSet) 1
                caveActual = drawCave processActual
                caveExpected =
                  T.intercalate
                    "\n"
                    [ "...####"
                    , "#######"
                    ]
            in do 
              heightActual @?= 1
              caveActual @?= caveExpected

        , testCase "Iteration 2" $
            let heightActual = efficientHeight caveFloor rtsSet windSet 2
                processActual = towerProcess caveFloor (makeInf rtsSet) (makeInf windSet) 2
                caveActual = drawCave processActual
                caveExpected =
                  T.intercalate
                    "\n"
                    [ "....#.."
                    , "....#.."
                    , "....#.."
                    , "....#.."
                    , "...####"
                    , "#######"
                    ]
            in do 
              heightActual @?= 5
              caveActual @?= caveExpected

        ,  testCase "Iteration 3" $
            let heightActual = efficientHeight caveFloor rtsSet windSet 3
                processActual = towerProcess caveFloor (makeInf rtsSet) (makeInf windSet) 3
                caveActual = drawCave processActual
                caveExpected =
                  T.intercalate
                    "\n"
                    [ "..####."
                    , "....#.."
                    , "....#.."
                    , "....#.."
                    , "....#.."
                    , "...####"
                    , "#######"
                    ]
            in do 
              heightActual @?= 6
              caveActual @?= caveExpected

      , testCase "Iteration 4" $
          let heightActual = efficientHeight caveFloor rtsSet windSet 4
              processActual = towerProcess caveFloor (makeInf rtsSet) (makeInf windSet) 4
              caveActual = drawCave processActual
              caveExpected =
                T.intercalate
                  "\n"
                  [ "....#.."
                  , "....#.."
                  , "....#.."
                  , "....#.."
                  , "..####."
                  , "....#.."
                  , "....#.."
                  , "....#.."
                  , "....#.."
                  , "...####"
                  , "#######"
                  ]
          in do 
            heightActual @?= 10
            caveActual @?= caveExpected

      , testCase "Iteration 5" $
          let heightActual = efficientHeight caveFloor rtsSet windSet 5
              processActual = towerProcess caveFloor (makeInf rtsSet) (makeInf windSet) 5
              caveActual = drawCave processActual
              caveExpected =
                T.intercalate
                  "\n"
                  [ "...####"
                  , "....#.."
                  , "....#.."
                  , "....#.."
                  , "....#.."
                  , "..####."
                  , "....#.."
                  , "....#.."
                  , "....#.."
                  , "....#.."
                  , "...####"
                  , "#######"
                  ]
          in do 
            heightActual @?= 11
            caveActual @?= caveExpected

      ,  testCase "Iteration 6" $
            let heightActual = efficientHeight caveFloor rtsSet windSet 6
                processActual = towerProcess caveFloor (makeInf rtsSet) (makeInf windSet) 6
                caveActual = drawCave processActual
                caveExpected =
                  T.intercalate
                    "\n"
                    [ "...####"
                    , "...##.."
                    , "...##.."
                    , "...##.."
                    , "...##.."
                    , "..####."
                    , "....#.."
                    , "....#.."
                    , "....#.."
                    , "....#.."
                    , "...####"
                    , "#######"
                    ]
            in do 
              heightActual @?= 11
              caveActual @?= caveExpected

      ,  testCase "Iteration 7" $
            let heightActual = efficientHeight caveFloor rtsSet windSet 7
                processActual = towerProcess caveFloor (makeInf rtsSet) (makeInf windSet) 7
                caveActual = drawCave processActual
                caveExpected =
                  T.intercalate
                    "\n"
                    [ "..####."
                    , "...####"
                    , "...##.."
                    , "...##.."
                    , "...##.."
                    , "...##.."
                    , "..####."
                    , "....#.."
                    , "....#.."
                    , "....#.."
                    , "....#.."
                    , "...####"
                    , "#######"
                    ]
            in do 
              heightActual @?= 12
              caveActual @?= caveExpected

      , testCase "Iteration 8" $
            let heightActual = efficientHeight caveFloor rtsSet windSet 8
                processActual = towerProcess caveFloor (makeInf rtsSet) (makeInf windSet) 8
                caveActual = drawCave processActual
                caveExpected =
                  T.intercalate
                    "\n"
                    [ "....#.."
                    , "....#.."
                    , "....#.."
                    , "....#.."
                    , "..####."
                    , "...####"
                    , "...##.."
                    , "...##.."
                    , "...##.."
                    , "...##.."
                    , "..####."
                    , "....#.."
                    , "....#.."
                    , "....#.."
                    , "....#.."
                    , "...####"
                    , "#######"
                    ]
            in do 
              heightActual @?= 16
              caveActual @?= caveExpected

      , testCase "Iteration 9" $
          let heightActual = efficientHeight caveFloor rtsSet windSet 9
              processActual = towerProcess caveFloor (makeInf rtsSet) (makeInf windSet) 9
              caveActual = drawCave processActual
              caveExpected =
                T.intercalate
                  "\n"
                  [ "...####"
                  , "....#.."
                  , "....#.."
                  , "....#.."
                  , "....#.."
                  , "..####."
                  , "...####"
                  , "...##.."
                  , "...##.."
                  , "...##.."
                  , "...##.."
                  , "..####."
                  , "....#.."
                  , "....#.."
                  , "....#.."
                  , "....#.."
                  , "...####"
                  , "#######"
                  ]
          in do
            heightActual @?= 17
            caveActual @?= caveExpected


    , testCase "Iteration 10" $
        let heightActual = efficientHeight caveFloor rtsSet windSet 10
            processActual = towerProcess caveFloor (makeInf rtsSet) (makeInf windSet) 10
            caveActual = drawCave processActual
            caveExpected =
              T.intercalate
                "\n"
                [ "...####"
                , "...##.."
                , "...##.."
                , "...##.."
                , "...##.."
                , "..####."
                , "...####" -- repeating variant (end)        [11]
                , "...##.."
                , "...##.."
                , "...##.."
                , "...##.."
                , "..####." -- repeating variant (start)      [6]
                , "....#.." -- initial height from cave floor [5]
                , "....#.."
                , "....#.."
                , "....#.."
                , "...####"
                , "#######"
                ]
        in do 
          heightActual @?= 17
          caveActual @?= caveExpected
        ]
    
    
oneWindTests :: TestTree
oneWindTests = 
  let windSet = East :| []
      rtsSet = HLine :| []

  in testGroup "One Wind, One Shape" $
        [ testCase "Iteration 0" $
            let heightActual = efficientHeight caveFloor rtsSet windSet 0
                processActual = towerProcess caveFloor (makeInf rtsSet) (makeInf windSet) 0
                caveActual = drawCave processActual
            in do 
              heightActual @?= 0
              caveActual @?= "#######"

        , testCase "Iteration 1" $
            let heightActual = efficientHeight caveFloor rtsSet windSet 1
                processActual = towerProcess caveFloor (makeInf rtsSet) (makeInf windSet) 1
                caveActual = drawCave processActual
                caveExpected =
                  T.intercalate
                    "\n"
                    [ "...####"
                    , "#######"
                    ]
            in do 
              heightActual @?= 1
              caveActual @?= caveExpected

        , testCase "Iteration 2" $
            let heightActual = efficientHeight caveFloor rtsSet windSet 2
                processActual = towerProcess caveFloor (makeInf rtsSet) (makeInf windSet) 2
                caveActual = drawCave processActual
                caveExpected =
                  T.intercalate
                    "\n"
                    [ "...####"
                    , "...####"
                    , "#######"
                    ]
            in do 
              heightActual @?= 2
              caveActual @?= caveExpected

        , testCase "Iteration 100" $
            let heightActual = efficientHeight caveFloor rtsSet windSet 100
            in heightActual @?= 100

        , testCase "Iteration 10000000" $
            let heightActual = efficientHeight caveFloor rtsSet windSet 10000000
            in heightActual @?= 10000000

        ]
    
    
solutionTests :: TestTree
solutionTests = 
  testGroup "Puzzle Solutions" $
    [ testCase "Part 1 - Puzzle Input" $
        let input = day17Input
            winds = case NE.nonEmpty (parse input) of
              Nothing -> error "Cannot parse wind input"
              Just wds -> makeInf wds
            rts = makeInf $ HLine :| [Plus .. Square]
            actual = calculateHeight $ towerProcess caveFloor rts winds 2022
            expected = 3163
        in actual @?= expected

    , testCase "Part 1 - Efficient Height - Example Input" $
        let input = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
            windSet = case NE.nonEmpty (parse input) of
              Nothing -> error "Cannot parse wind input"
              Just wds -> wds
            rtsSet = HLine :| [Plus .. Square]
            actual = efficientHeight caveFloor rtsSet windSet 2022
            expected = 3068
        in actual @?= expected

    , testCase "Part 2 - Example Input" $
        let input = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
            winds = case NE.nonEmpty (parse input) of
              Nothing -> error "Cannot parse wind input"
              Just wds -> makeInf wds
            rts = makeInf $ HLine :| [Plus .. Square]
            actual = calculateHeight $ towerProcess caveFloor rts winds 10000
            expected = 1
        in actual @?= expected
    ]

