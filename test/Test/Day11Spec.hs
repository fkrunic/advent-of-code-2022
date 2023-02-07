module Test.Day11Spec (spec) where

import Test.Hspec (SpecWith, describe, it, shouldBe)

import Data.Either (fromRight)
import Data.List (sort)
import Data.Map (Map, elems, fromList)
import Data.Text (Text)
import Data.Text qualified as T
import Day11
import Text.Megaparsec (runParser, some)
import Prelude hiding (round)

spec :: SpecWith ()
spec =
  describe "Day 11" $ do
    it "Parsing Puzzle Input" $ do
      parser puzzleInput `shouldBe` [m0, m1, m2, m3]

    it "First Round" $ do
      let expectedItems =
            [ [Item 20, Item 23, Item 27, Item 26]
            , [Item 2080, Item 25, Item 167, Item 207, Item 401, Item 1046]
            , []
            , []
            ]
          actual = runRounds labels props 1 (getItems monkeys)
      map holding (elems actual) `shouldBe` expectedItems

    it "Twenty Rounds" $ do
      let expectedItems =
            [ [Item 10, Item 12, Item 14, Item 26, Item 34]
            , [Item 245, Item 93, Item 53, Item 199, Item 115]
            , []
            , []
            ]
          expectedCounters = [101, 95, 7, 105]
          actual = runRounds labels props 20 (getItems monkeys)
          actualHolding = map holding (elems actual)
          actualCounters = map counter (elems actual)

      actualHolding `shouldBe` expectedItems
      actualCounters `shouldBe` expectedCounters
      monkeyBusiness actualCounters `shouldBe` 10605

monkeyBusiness :: [Int] -> Int
monkeyBusiness counts = head ordered * ordered !! 1
 where
  ordered = reverse $ sort counts

monkeys :: [Monkey]
monkeys = [m0, m1, m2, m3]

labels :: [Label]
labels = map label monkeys

props :: Map Label Monkey
props = fromList $ map (\m -> (label m, m)) monkeys

parser :: Text -> [Monkey]
parser = fromRight [] . runParser (some pMonkey) ""

m0, m1, m2, m3 :: Monkey
m0 =
  Monkey
    { label = Label 0
    , items = [Item 79, Item 98]
    , operation = Multiply (Just 19)
    , divisor = 23
    , throwChoices = (Label 2, Label 3)
    }
m1 =
  Monkey
    { label = Label 1
    , items = [Item 54, Item 65, Item 75, Item 74]
    , operation = Add (Just 6)
    , divisor = 19
    , throwChoices = (Label 2, Label 0)
    }
m2 =
  Monkey
    { label = Label 2
    , items = [Item 79, Item 60, Item 97]
    , operation = Multiply Nothing
    , divisor = 13
    , throwChoices = (Label 1, Label 3)
    }
m3 =
  Monkey
    { label = Label 3
    , items = [Item 74]
    , operation = Add (Just 3)
    , divisor = 17
    , throwChoices = (Label 0, Label 1)
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
