module Test.Day17Spec where

import Data.List.NonEmpty as NE
import Data.Text qualified as T
import Problems.Day17
import Test.Hspec
import Test.Input
import Utilities.Grids
import Utilities.Infinites

spec :: SpecWith ()
spec = do
  describe "Day 17 Tests" $ do
    it "Parsing wind direction" $ do
      let actual = parse ">>><<"
          expected = [East, East, East, West, West]
      actual `shouldBe` expected

    describe "Cave Tests" $ do
      let input = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
          winds = case NE.nonEmpty (parse input) of
            Nothing -> error "Cannot parse wind input"
            Just wds -> makeInf wds
          rts = makeInf $ HLine :| [Plus .. Square]

      it "Can draw the initial cave" $ do
        let actual = drawCave caveFloor
            expected = "#######"
        actual `shouldBe` expected

      describe "Rock Process" $ do
        it "Rock 1" $ do
          let rp = startingPos caveFloor HLine
              (settled, _) = rockProcess caveFloor rp winds
              actual = drawCave settled
              expected =
                T.intercalate
                  "\n"
                  [ "..####."
                  , "#######"
                  ]
          actual `shouldBe` expected

        it "Rock 2" $ do
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
          nextWind `shouldBe` West
          actual `shouldBe` expected

      describe "Shapes" $ do
        it "Shifting anchor of square" $ do
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
          actual `shouldBe` expected

      describe "Starting Positions" $ do
        it "Starting Position - Rock 1" $ do
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
          actual `shouldBe` expected

        it "Starting Position - Rock 2" $ do
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
          actual `shouldBe` expected

      describe "Tower Process" $ do
        let process = towerProcess caveFloor rts winds

        it "Rock 1" $ do
          let actual = drawCave $ process 1
              expected =
                T.intercalate
                  "\n"
                  [ "..####."
                  , "#######"
                  ]
          actual `shouldBe` expected

        it "Rock 2" $ do
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
          actual `shouldBe` expected

        it "Rock 3" $ do
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
          actual `shouldBe` expected

        it "Rock 4" $ do
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
          actual `shouldBe` expected

        it "Rock 5" $ do
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
          actual `shouldBe` expected

        it "Rock 6" $ do
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
          actual `shouldBe` expected

        it "Rock 7" $ do
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
          actual `shouldBe` expected

        it "Rock 8" $ do
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
          actual `shouldBe` expected

        it "Rock 9" $ do
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
          actual `shouldBe` expected

        it "Rock 10" $ do
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
          actual `shouldBe` expected

      describe "Efficient Height" $ do
        describe "Multiple Winds, Multiple Shapes (LCM=3x2=6)" $ do
          let windSet = East :| [East, West]
              rtsSet = HLine :| [VLine]

          it "Iteration 0" $ do
            let heightActual = efficientHeight caveFloor rtsSet windSet 0
                processActual = towerProcess caveFloor (makeInf rtsSet) (makeInf windSet) 0
                caveActual = drawCave processActual
            heightActual `shouldBe` 0
            caveActual `shouldBe` "#######"

          it "Iteration 1" $ do
            let heightActual = efficientHeight caveFloor rtsSet windSet 1
                processActual = towerProcess caveFloor (makeInf rtsSet) (makeInf windSet) 1
                caveActual = drawCave processActual
                caveExpected =
                  T.intercalate
                    "\n"
                    [ "...####"
                    , "#######"
                    ]
            heightActual `shouldBe` 1
            caveActual `shouldBe` caveExpected

          it "Iteration 2" $ do
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
            heightActual `shouldBe` 5
            caveActual `shouldBe` caveExpected

          it "Iteration 3" $ do
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
            heightActual `shouldBe` 6
            caveActual `shouldBe` caveExpected

          it "Iteration 4" $ do
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
            heightActual `shouldBe` 10
            caveActual `shouldBe` caveExpected

          it "Iteration 5" $ do
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
            heightActual `shouldBe` 11
            caveActual `shouldBe` caveExpected

          it "Iteration 6" $ do
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
            heightActual `shouldBe` 11
            caveActual `shouldBe` caveExpected

          it "Iteration 7" $ do
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
            heightActual `shouldBe` 12
            caveActual `shouldBe` caveExpected

          it "Iteration 8" $ do
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
            heightActual `shouldBe` 16
            caveActual `shouldBe` caveExpected

          it "Iteration 9" $ do
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
            heightActual `shouldBe` 17
            caveActual `shouldBe` caveExpected

          it "Iteration 10" $ do
            pendingWith "Patterns need to be identified dynamically"
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
            heightActual `shouldBe` 17
            caveActual `shouldBe` caveExpected

        describe "One Wind, One Shape" $ do
          let windSet = East :| []
              rtsSet = HLine :| []

          it "Iteration 0" $ do
            let heightActual = efficientHeight caveFloor rtsSet windSet 0
                processActual = towerProcess caveFloor (makeInf rtsSet) (makeInf windSet) 0
                caveActual = drawCave processActual
            heightActual `shouldBe` 0
            caveActual `shouldBe` "#######"

          it "Iteration 1" $ do
            pendingWith "Patterns need to be identified dynamically"
            let heightActual = efficientHeight caveFloor rtsSet windSet 1
                processActual = towerProcess caveFloor (makeInf rtsSet) (makeInf windSet) 1
                caveActual = drawCave processActual
                caveExpected =
                  T.intercalate
                    "\n"
                    [ "...####"
                    , "#######"
                    ]
            heightActual `shouldBe` 1
            caveActual `shouldBe` caveExpected

          it "Iteration 2" $ do
            pendingWith "Patterns need to be identified dynamically"
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
            heightActual `shouldBe` 2
            caveActual `shouldBe` caveExpected

          it "Iteration 100" $ do
            pendingWith "Patterns need to be identified dynamically"
            let heightActual = efficientHeight caveFloor rtsSet windSet 100
            heightActual `shouldBe` 100

          it "Iteration 10000000" $ do
            pendingWith "Patterns need to be identified dynamically"
            let heightActual = efficientHeight caveFloor rtsSet windSet 10000000
            heightActual `shouldBe` 10000000

      describe "Puzzle Solutions" $ do
        it "Part 1 - Puzzle Input" $ do
          pendingWith "Working"
          let input = day17Input
              winds = case NE.nonEmpty (parse input) of
                Nothing -> error "Cannot parse wind input"
                Just wds -> makeInf wds
              rts = makeInf $ HLine :| [Plus .. Square]
              actual = calculateHeight $ towerProcess caveFloor rts winds 2022
              expected = 3163
          actual `shouldBe` expected

        it "Part 1 - Efficient Height - Example Input" $ do
          pendingWith "Broken"
          let input = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
              windSet = case NE.nonEmpty (parse input) of
                Nothing -> error "Cannot parse wind input"
                Just wds -> wds
              rtsSet = HLine :| [Plus .. Square]
              actual = efficientHeight caveFloor rtsSet windSet 2022
              expected = 3068
          actual `shouldBe` expected

        it "Part 2 - Example Input" $ do
          pendingWith "Too long"
          let input = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
              winds = case NE.nonEmpty (parse input) of
                Nothing -> error "Cannot parse wind input"
                Just wds -> makeInf wds
              rts = makeInf $ HLine :| [Plus .. Square]
              actual = calculateHeight $ towerProcess caveFloor rts winds 10000
              expected = 1
          actual `shouldBe` expected
