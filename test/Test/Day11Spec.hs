module Test.Day11Spec (spec) where

import Test.Hspec (SpecWith, describe, it, shouldBe)

import Data.Either (fromRight)
import Data.Text (Text)
import Data.Text qualified as T
import Day11
import Text.Megaparsec (runParser, some)

parser :: Text -> [Monkey]
parser = fromRight [] . runParser (some pMonkey) ""

spec :: SpecWith ()
spec =
  describe "Day 11" $ do
    it "Parsing Puzzle Input" $ do
      parser puzzleInput `shouldBe` [m0, m1, m2, m3]
    describe "First Round of Scattering" $ do
      it "Monkey 0" $ do
        scatter m0 `shouldBe` [(Item 500, Label 3), (Item 620, Label 3)]
      it "Monkey 1" $ do
        scatter m1
          `shouldBe` [ (Item 20, Label 0)
                     , (Item 23, Label 0)
                     , (Item 27, Label 0)
                     , (Item 26, Label 0)
                     ]
      it "Monkey 2" $ do
        scatter m2
          `shouldBe` [ (Item 2080, Label 1)
                     , (Item 1200, Label 3)
                     , (Item 3136, Label 3)
                     ]
      it "Monkey 3" $ do
        scatter m3
          `shouldBe` [ (Item 25, Label 1)
                     , (Item 167, Label 1)
                     , (Item 207, Label 1)
                     , (Item 401, Label 1)
                     , (Item 1046, Label 1)
                     ]

m0, m1, m2, m3 :: Monkey
m0 =
  Monkey
    { label = Label 0
    , startingItems = [Item 79, Item 98]
    , operation = Multiply (Just 19)
    , divisibility = 23
    , throwChoice = (Label 2, Label 3)
    }
m1 =
  Monkey
    { label = Label 1
    , startingItems = [Item 54, Item 65, Item 75, Item 74]
    , operation = Add (Just 6)
    , divisibility = 19
    , throwChoice = (Label 2, Label 0)
    }
m2 =
  Monkey
    { label = Label 2
    , startingItems = [Item 79, Item 60, Item 97]
    , operation = Multiply Nothing
    , divisibility = 13
    , throwChoice = (Label 1, Label 3)
    }
m3 =
  Monkey
    { label = Label 3
    , startingItems = [Item 74]
    , operation = Add (Just 3)
    , divisibility = 17
    , throwChoice = (Label 0, Label 1)
    }

puzzleInput :: Text
puzzleInput =
  T.intercalate
    "\n"
    [ "Monkey 0:"
    , "  Starting items: 79, 98"
    , "  Operation: new = old * 19"
    , "  Test: divisible by 23"
    , "    If true: throw to monkey 2"
    , "    If false: throw to monkey 3"
    , ""
    , "Monkey 1:"
    , "  Starting items: 54, 65, 75, 74"
    , "  Operation: new = old + 6"
    , "  Test: divisible by 19"
    , "    If true: throw to monkey 2"
    , "    If false: throw to monkey 0"
    , ""
    , "Monkey 2:"
    , "  Starting items: 79, 60, 97"
    , "  Operation: new = old * old"
    , "  Test: divisible by 13"
    , "    If true: throw to monkey 1"
    , "    If false: throw to monkey 3"
    , ""
    , "Monkey 3:"
    , "  Starting items: 74"
    , "  Operation: new = old + 3"
    , "  Test: divisible by 17"
    , "    If true: throw to monkey 0"
    , "    If false: throw to monkey 1"
    ]