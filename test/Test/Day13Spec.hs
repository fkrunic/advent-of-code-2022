module Test.Day13Spec where

import Data.Either (fromRight)
import Data.Text (Text)
import Data.Text qualified as T
import Day13
import Test.Hspec
import Text.Megaparsec
import Text.Megaparsec.Char

spec :: SpecWith ()
spec =
  describe "Day 13 Tests" $ do
    describe "Parsing Tests" $ do
      it "Single 5" $ do
        runParser pInt "" "5" `shouldBe` Right (CInt 5)

      it "Single [1,1,3,1,1]" $ do
        runParser pList "" "[1,1,3,1,1]" `shouldBe` Right (fst e1)

      it "Pair [1,1,3,1,1], [1,1,5,1,1]" $ do
        runParser pPair "" "[1,1,3,1,1]\n[1,1,5,1,1]" `shouldBe` Right e1

      it "Pair [1,[2,[3,[4,[5,6,7]]]],8,9], [1,[2,[3,[4,[5,6,0]]]],8,9]" $ do
        runParser pPair "" "[1,[2,[3,[4,[5,6,7]]]],8,9]\n[1,[2,[3,[4,[5,6,0]]]],8,9]" `shouldBe` Right e8

      it "All Pairs Input" $ do
        parser exampleInput `shouldBe` [e1, e2, e3, e4, e5, e6, e7, e8]

    describe "Validity Tests" $ do
      it "Valid Example - [1,1,3,1,1], [1,1,5,1,1]" $ do
        let (left, right) = e1
        valid left right `shouldBe` True

      it "Invalid Example - [[1],[2,3,4]], [[1],4]" $ do
        let (left, right) = e2
        valid left right `shouldBe` False

      it "Invalid Example - [9], [[8, 7, 6]]" $ do
        let (left, right) = e3
        valid left right `shouldBe` False

      it "Valid Example - [[4,4],4,4], [[4,4],4,4,4]" $ do
        let (left, right) = e4
        valid left right `shouldBe` True

      it "Invalid Example - [7,7,7,7], [7,7,7]" $ do
        let (left, right) = e5
        valid left right `shouldBe` False

      it "Valid Example - [], [3]" $ do
        let (left, right) = e6
        valid left right `shouldBe` True

      it "Invalid Example - [[[]]], [[]]" $ do
        let (left, right) = e7
        valid left right `shouldBe` False

      it "Invalid Example - [1,[2,[3,[4,[5,6,7]]]],8,9], [1,[2,[3,[4,[5,6,0]]]],8,9]" $ do
        let (left, right) = e8
        valid left right `shouldBe` False

parser :: Text -> [(Comparison, Comparison)]
parser = fromRight [] . runParser (some (pPair <* space)) ""

e1, e2, e3, e4, e5, e6, e7, e8 :: (Comparison, Comparison)
e1 = (left, right)
 where
  left = CList $ map CInt [1, 1, 3, 1, 1]
  right = CList $ map CInt [1, 1, 5, 1, 1]
e2 = (left, right)
 where
  left =
    CList
      [ CList [CInt 1]
      , CList [CInt 2, CInt 3, CInt 4]
      ]
  right =
    CList
      [ CList [CInt 1]
      , CInt 4
      ]
e3 = (left, right)
 where
  left = CList [CInt 9]
  right = CList [CList [CInt 8, CInt 7, CInt 6]]
e4 = (left, right)
 where
  left =
    CList
      [ CList [CInt 4, CInt 4]
      , CInt 4
      , CInt 4
      ]
  right =
    CList
      [ CList [CInt 4, CInt 4]
      , CInt 4
      , CInt 4
      , CInt 4
      ]
e5 = (left, right)
 where
  left = CList $ map CInt [7, 7, 7, 7]
  right = CList $ map CInt [7, 7, 7]
e6 = (left, right)
 where
  left = CList []
  right = CList [CInt 3]
e7 = (left, right)
 where
  left = CList [CList [CList []]]
  right = CList [CList []]
e8 = (left, right)
 where
  left =
    CList
      [ CInt 1
      , CList
          [ CInt 2
          , CList
              [ CInt 3
              , CList
                  [ CInt 4
                  , CList
                      [CInt 5, CInt 6, CInt 7]
                  ]
              ]
          ]
      , CInt 8
      , CInt 9
      ]
  right =
    CList
      [ CInt 1
      , CList
          [ CInt 2
          , CList
              [ CInt 3
              , CList
                  [ CInt 4
                  , CList
                      [CInt 5, CInt 6, CInt 0]
                  ]
              ]
          ]
      , CInt 8
      , CInt 9
      ]

exampleInput :: Text
exampleInput =
  T.intercalate
    "\n"
    [ "[1,1,3,1,1]"
    , "[1,1,5,1,1]"
    , ""
    , "[[1],[2,3,4]]"
    , "[[1],4]"
    , ""
    , "[9]"
    , "[[8,7,6]]"
    , ""
    , "[[4,4],4,4]"
    , "[[4,4],4,4,4]"
    , ""
    , "[7,7,7,7]"
    , "[7,7,7]"
    , ""
    , "[]"
    , "[3]"
    , ""
    , "[[[]]]"
    , "[[]]"
    , ""
    , "[1,[2,[3,[4,[5,6,7]]]],8,9]"
    , "[1,[2,[3,[4,[5,6,0]]]],8,9]"
    ]