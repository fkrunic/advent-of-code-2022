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
