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