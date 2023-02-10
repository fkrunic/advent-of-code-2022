module Test.Day13Spec where

import Day13
import Test.Hspec

spec :: SpecWith ()
spec =
  describe "Day 13 Tests" $ do
    it "Valid Example - [1,1,3,1,1], [1,1,5,1,1]" $ do
      let left = CList $ map CInt [1, 1, 3, 1, 1]
          right = CList $ map CInt [1, 1, 5, 1, 1]
      valid left right `shouldBe` True

    it "Invalid Example - [[1],[2,3,4]], [[1],4]" $ do
      let left =
            CList
              [ CList [CInt 1]
              , CList [CInt 2, CInt 3, CInt 4]
              ]
          right =
            CList
              [ CList [CInt 1]
              , CInt 4
              ]
      valid left right `shouldBe` False

    it "Invalid Example - [9], [[8, 7, 6]]" $ do
      let left = CList [CInt 9]
          right = CList [CList [CInt 8, CInt 7, CInt 6]]
      valid left right `shouldBe` False

    it "Valid Example - [[4,4],4,4], [[4,4],4,4,4]" $ do
      let left =
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
      valid left right `shouldBe` True

    it "Invalid Example - [7,7,7,7], [7,7,7]" $ do
      let left = CList $ map CInt [7, 7, 7, 7]
          right = CList $ map CInt [7, 7, 7]
      valid left right `shouldBe` False

    it "Valid Example - [], [3]" $ do
      let left = CList []
          right = CList [CInt 3]
      valid left right `shouldBe` True

    it "Invalid Example - [[[]]], [[]]" $ do
      let left = CList [CList [CList []]]
          right = CList [CList []]
      valid left right `shouldBe` False

    it "Invalid Example - [1,[2,[3,[4,[5,6,7]]]],8,9], [1,[2,[3,[4,[5,6,0]]]],8,9]" $ do
      let left =
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
      valid left right `shouldBe` False