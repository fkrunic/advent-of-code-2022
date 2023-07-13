module Test.Day11Spec (spec) where

import Data.Either (fromRight)
import Data.List (sort)
import Data.Map (Map, elems, fromList)
import Data.Text (Text)
import Data.Text qualified as T
import Problems.Day11
import Text.Megaparsec (runParser, some)
import Prelude hiding (round)

import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec =
  testGroup "Day 11" $
    [ testCase "Parsing Puzzle Input" $
        parser puzzleInput @?= [m0, m1, m2, m3]

    , testCase "First Round" $
        let expectedItems =
              [ [Item 20, Item 23, Item 27, Item 26]
              , [Item 2080, Item 25, Item 167, Item 207, Item 401, Item 1046]
              , []
              , []
              ]
            actual = runRounds 
              (Reducer 3)
              (Times 1)
              (labels exMonkeys) 
              (props exMonkeys) 
              (getItems exMonkeys)

        in map holding (elems actual) @?= expectedItems

    , testCase "Twenty Rounds" $
        let expectedItems =
              [ [Item 10, Item 12, Item 14, Item 26, Item 34]
              , [Item 245, Item 93, Item 53, Item 199, Item 115]
              , []
              , []
              ]
            expectedCounters = [101, 95, 7, 105]
            actual = runRounds 
              (Reducer 3)
              (Times 20)
              (labels exMonkeys) 
              (props exMonkeys) 
              (getItems exMonkeys)

            actualHolding = map holding (elems actual)
            actualCounters = map counter (elems actual)

        in (actualHolding, actualCounters, monkeyBusiness actualCounters)
              @?= (expectedItems, expectedCounters, 10605)

    , testCase "1 Round - No Reducer" $
        let expectedCounters = [2, 4, 3, 6]
            actual = runRounds 
              (Reducer 1) 
              (Times 1) 
              (labels exMonkeys) 
              (props exMonkeys) (
                getItems exMonkeys)
            actualCounters = map counter (elems actual)
        in actualCounters @?= expectedCounters

    , testCase "20 Rounds - No Reducer" $
        let expectedCounters = [99, 97, 8, 103]
            actual = runRounds 
              (Reducer 1) 
              (Times 20) 
              (labels exMonkeys) 
              (props exMonkeys) 
              (getItems exMonkeys)
            actualCounters = map counter (elems actual)
        in actualCounters @?= expectedCounters          

    , testGroup "Counting Game Tests" $
        [ testCase "Generate Indexed Items" $
            generateIndexedItems exMonkeys @?=
              [ (Label 0, Index 0, Worry 79)
              , (Label 0, Index 1, Worry 98)
              , (Label 1, Index 2, Worry 54)
              , (Label 1, Index 3, Worry 65)
              , (Label 1, Index 4, Worry 75)
              , (Label 1, Index 5, Worry 74)
              , (Label 2, Index 6, Worry 79)
              , (Label 2, Index 7, Worry 60)
              , (Label 2, Index 8, Worry 97)
              , (Label 3, Index 9, Worry 74)
              ]

        , testCase "1000 Rounds - Residue Implementation" $
            let expectedCounters = map Counter [5204, 4792, 199, 5192]
                indexedItems = generateIndexedItems exMonkeys
                factors = getFactors exMonkeys
                residuals = buildResiduals indexedItems factors
                state = initialIndexedState exMonkeys
                actual = runResidues 
                  (Times 1000) 
                  (labels exMonkeys) 
                  (props exMonkeys) 
                  (state, residuals)
                actualCounters = map residueCounter $ elems actual
            in actualCounters @?= expectedCounters  

        , testCase "10,000 Rounds - Residue Implementation" $
            let expectedCounters = map Counter [52166, 47830, 1938, 52013]
                indexedItems = generateIndexedItems exMonkeys
                factors = getFactors exMonkeys
                residuals = buildResiduals indexedItems factors
                state = initialIndexedState exMonkeys
                actual = runResidues 
                  (Times 10000) 
                  (labels exMonkeys) 
                  (props exMonkeys) 
                  (state, residuals)
                actualCounters = map residueCounter $ elems actual
            in actualCounters @?= expectedCounters              
        ]

    , testGroup "Puzzle Solutions" $
        [ testCase "Part 1 Solution" $
             part1Solution part1Input @?= 58322       

        , testCase "Part 2 Solution" $ do
            part2Solution part1Input @?= 13937702909
        ]
    ]
    
part1Solution :: Text -> Int
part1Solution =
  monkeyBusiness . map counter . elems . runner . parser
 where
  runner mks = runRounds 
    (Reducer 3) 
    (Times 20) 
    (labels mks)
    (props mks) 
    (getItems mks)

part2Solution :: Text -> Int
part2Solution =
  monkeyBusiness . map (unpack . residueCounter) . elems . runner . parser
  where
    unpack (Counter c) = c
    runner mks = 
      runResidues 
        (Times 10000) 
        (labels mks) 
        (props mks) 
        (state, residuals)      
      where
        indexedItems = generateIndexedItems mks
        factors = getFactors mks
        residuals = buildResiduals indexedItems factors
        state = initialIndexedState mks        

monkeyBusiness :: [Int] -> Int
monkeyBusiness counts = head ordered * ordered !! 1
 where
  ordered = reverse $ sort counts

exMonkeys :: [Monkey]
exMonkeys = [m0, m1, m2, m3]

labels :: [Monkey] -> [Label]
labels = map label

props :: [Monkey] -> Map Label Monkey
props = fromList . map (\m -> (label m, m))

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

part1Input :: Text
part1Input =
  T.intercalate
    "\n"
    [ "Monkey 0:"
    , "  Starting items: 59, 65, 86, 56, 74, 57, 56"
    , "  Operation: new = old * 17"
    , "  Test: divisible by 3"
    , "    If true: throw to monkey 3"
    , "    If false: throw to monkey 6"
    , ""
    , "Monkey 1:"
    , "  Starting items: 63, 83, 50, 63, 56"
    , "  Operation: new = old + 2"
    , "  Test: divisible by 13"
    , "    If true: throw to monkey 3"
    , "    If false: throw to monkey 0"
    , ""
    , "Monkey 2:"
    , "  Starting items: 93, 79, 74, 55"
    , "  Operation: new = old + 1"
    , "  Test: divisible by 2"
    , "    If true: throw to monkey 0"
    , "    If false: throw to monkey 1"
    , ""
    , "Monkey 3:"
    , "  Starting items: 86, 61, 67, 88, 94, 69, 56, 91"
    , "  Operation: new = old + 7"
    , "  Test: divisible by 11"
    , "    If true: throw to monkey 6"
    , "    If false: throw to monkey 7"
    , ""
    , "Monkey 4:"
    , "  Starting items: 76, 50, 51"
    , "  Operation: new = old * old"
    , "  Test: divisible by 19"
    , "    If true: throw to monkey 2"
    , "    If false: throw to monkey 5"
    , ""
    , "Monkey 5:"
    , "  Starting items: 77, 76"
    , "  Operation: new = old + 8"
    , "  Test: divisible by 17"
    , "    If true: throw to monkey 2"
    , "    If false: throw to monkey 1"
    , ""
    , "Monkey 6:"
    , "  Starting items: 74"
    , "  Operation: new = old * 2"
    , "  Test: divisible by 5"
    , "    If true: throw to monkey 4"
    , "    If false: throw to monkey 7"
    , ""
    , "Monkey 7:"
    , "  Starting items: 86, 85, 52, 86, 91, 95"
    , "  Operation: new = old + 6"
    , "  Test: divisible by 7"
    , "    If true: throw to monkey 4"
    , "    If false: throw to monkey 5"
    ]
