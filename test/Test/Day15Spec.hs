module Test.Day15Spec (spec) where

import Test.Hspec

import Data.Bifunctor (bimap)
import Data.Either (fromRight)
import Data.Text (Text)
import Data.Text qualified as T
import Day15

import Grids

import Text.Megaparsec

spec :: SpecWith ()
spec =
  describe "Day 15 Tests" $ do
    it "Parse Single Beacon Line" $ do
      let line = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
          actual = runParser pLine "" line
          expected = head exampleSpread
      actual `shouldBe` Right expected

    it "Parses Example Input" $ do
      parser exampleInput `shouldBe` exampleSpread

parser :: Text -> [(SensorLocation, BeaconLocation)]
parser = fromRight [] . runParser (some pLine) ""

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

exampleSpread :: [(SensorLocation, BeaconLocation)]
exampleSpread =
  map
    (bimap SensorLocation BeaconLocation)
    [ (point 2 18, point (-2) 15)
    , (point 9 16, point 10 16)
    , (point 13 2, point 15 3)
    , (point 12 14, point 10 16)
    , (point 10 20, point 10 16)
    , (point 14 17, point 10 16)
    , (point 8 7, point 2 10)
    , (point 2 0, point 2 10)
    , (point 0 11, point 2 10)
    , (point 20 14, point 25 17)
    , (point 17 20, point 21 22)
    , (point 16 7, point 15 3)
    , (point 14 3, point 15 3)
    , (point 20 1, point 15 3)
    ]