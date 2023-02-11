module Test.Day14Spec (spec) where

import Day14
import Grids
import Test.Hspec
import Text.Megaparsec

spec :: SpecWith ()
spec =
  describe "Day 14 Tests" $ do
    it "Parsing" $ do
      let expected =
            [ (XCoordinate 498, YCoordinate 4)
            , (XCoordinate 498, YCoordinate 6)
            , (XCoordinate 496, YCoordinate 6)
            ]
          actual = runParser pDrawPath "" "498,4 -> 498,6 -> 496,6"
      actual `shouldBe` Right expected

    describe "Points Along Tests" $ do
      it "Points along (1,1) and (1,3)" $ do
        let start = (XCoordinate 1, YCoordinate 1)
            middle = (XCoordinate 1, YCoordinate 2)
            end = (XCoordinate 1, YCoordinate 3)
        pointsAlong start end `shouldBe` Just [start, middle, end]
        pointsAlong end start `shouldBe` Just [end, middle, start]

      it "Points along (4,1) and (1,1)" $ do
        let start = (XCoordinate 4, YCoordinate 1)
            m1 = (XCoordinate 3, YCoordinate 1)
            m2 = (XCoordinate 2, YCoordinate 1)
            end = (XCoordinate 1, YCoordinate 1)
        pointsAlong start end `shouldBe` Just [start, m1, m2, end]
        pointsAlong end start `shouldBe` Just [end, m2, m1, start]

      it "No points along (1,1) and (3,3)" $ do
        let start = (XCoordinate 1, YCoordinate 1)
            end = (XCoordinate 3, YCoordinate 3)
        pointsAlong start end `shouldBe` Nothing
        pointsAlong end start `shouldBe` Nothing

      it "Chaining points (1,1) -> (3,1) -> (3,3)" $ do
        let p1 = point 1 1
            p2 = point 2 1
            p3 = point 3 1
            p4 = point 3 2
            p5 = point 3 3
            expected = [p1, p2, p3, p4, p5]
        chainPath [p1, p3, p5] `shouldBe` Just expected

      it "Chaining points (5,1) -> (1,1) -> (1,5)" $ do
        chainPath [point 5 1, point 1 1, point 1 5]
          `shouldBe` Just
            [ point 5 1
            , point 4 1
            , point 3 1
            , point 2 1
            , point 1 1
            , point 1 2
            , point 1 3
            , point 1 4
            , point 1 5
            ]

      it "No points along (1,1) -> (4,1) -> (5,3)" $ do
        chainPath [point 1 1, point 4 1, point 5 3] `shouldBe` Nothing
