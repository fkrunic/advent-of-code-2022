module Test.Day17Spec where

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty as NE
import Data.Text qualified as T
import Day17
import Infinites
import Test.Hspec

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
          pendingWith "Need to fix rock process"
          let rp = startingPos caveFloor HLine
              (settled, _) = rockProcess caveFloor rp winds
              actual = drawCave settled
              expected = T.intercalate "\n"
                [ "..####."
                , "#######"
                ]
          actual `shouldBe` expected

      describe "Tower Process" $ do
        let process = towerProcess caveFloor rts winds

        it "Rock 1" $ do
          pendingWith "Need to fix tower process"
          let actual = drawCave $ process 3
              expected = T.intercalate "\n"
                [ "..####."
                , "#######"
                ]
          actual `shouldBe` expected
