module Test.Day14Spec (spec) where

import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Day14
import Grids
import Test.Hspec
import Text.Megaparsec

spec :: SpecWith ()
spec =
  describe "Day 14 Tests" $ do
    it "Parsing" $ do
      let expected =
            [ point 498 4
            , point 498 6
            , point 496 6
            ]
          actual = runParser pDrawPath "" "498,4 -> 498,6 -> 496,6"
      actual `shouldBe` Right expected

    describe "Points Along Tests" $ do
      it "Points along (1,1) and (1,3)" $ do
        let start = point 1 1
            middle = point 1 2
            end = point 1 3
        pointsAlong start end `shouldBe` Just [start, middle, end]
        pointsAlong end start `shouldBe` Just [end, middle, start]

      it "Points along (4,1) and (1,1)" $ do
        let start = point 4 1
            m1 = point 3 1
            m2 = point 2 1
            end = point 1 1
        pointsAlong start end `shouldBe` Just [start, m1, m2, end]
        pointsAlong end start `shouldBe` Just [end, m2, m1, start]

      it "No points along (1,1) and (3,3)" $ do
        let start = point 1 1
            end = point 3 3
        pointsAlong start end `shouldBe` Nothing
        pointsAlong end start `shouldBe` Nothing

      it "Chaining points (1,1) -> (3,1) -> (3,3)" $ do
        chainPath [point 1 1, point 3 1, point 3 3]
          `shouldBe` Just
            [ point 1 1
            , point 2 1
            , point 3 1
            , point 3 2
            , point 3 3
            ]

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

    describe "Rendering Grid" $ do
      it "Drawing a grid with no sand" $ do
        let paths =
              [ [point 498 4, point 498 6, point 496 6]
              , [point 503 4, point 502 4, point 502 9, point 494 9]
              ]
            grid = fromMaybe M.empty $ defineGrid paths
            actual = drawGrid grid
            expected =
              T.intercalate
                "\n"
                [ "............"
                , ".......+...."
                , "............"
                , "............"
                , "............"
                , ".....#...##."
                , ".....#...#.."
                , "...###...#.."
                , ".........#.."
                , ".........#.."
                , ".#########.."
                , "************"
                ]
        actual `shouldBe` expected