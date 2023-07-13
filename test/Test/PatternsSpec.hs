module Test.PatternsSpec (spec) where

import Data.List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit
import Utilities.Patterns

spec :: TestTree
spec = 
  testGroup "Pattern Tests" $ 
    [ testGroup "getPatterns" $
        [ testCase "''" $ 
            getPatterns "" @?= []

        , testCase "'a'" $ 
            getPatterns "a" @?= []

        , testCase "'ab'" $ 
            getPatterns "ab" @?= []

        , testCase "'aa'" $ 
            getPatterns "aa" @?= [(Offset 0, PatternLength 1)]

        , testCase "'aaa'" $ 
            let actual = getPatterns "aaa" 
                expected = 
                  [ (Offset 0, PatternLength 1)
                  , (Offset 1, PatternLength 1)
                  ]
            in actual @?= expected

        , testGroup "'9smxundu39[abc][abc]'" $ 
            [ testCase "chop" $ 
                chop 5 "[abc][abc]" @?= ["[abc]", "[abc]"]

            , testCase "group-chop" $ 
                group (chop 5 "[abc][abc]") @?= [["[abc]", "[abc]"]]

            , testCase "splitting" $ 
                splitAt 10 "9smxundu39[abc][abc]" @?= ("9smxundu39", "[abc][abc]")

            , testCase "hasPattern" $ 
                hasPattern (Offset 10) (PatternLength 5) "9smxundu39[abc][abc]" @?= True

            , testCase "getPatterns" $
                getPatterns "9smxundu39[abc][abc]" @?= [(Offset 10, PatternLength 5)]

            ]

        , testCase "'asfi3[12][12][12][12]'" $ 
            let actual = getPatterns "asfi3[12][12][12][12]"
                expected =  
                  [ (Offset 5, PatternLength 4)
                  , (Offset 5, PatternLength 8)
                  , (Offset 9, PatternLength 4)
                  , (Offset 13, PatternLength 4)
                  ]
            in actual @?= expected

        , testCase "Cave Rendering" $
            let render =
                  reverse $
                    T.lines $
                      T.intercalate
                        "\n"
                        [ "...####"
                        , "...##.."
                        , "...##.."
                        , "...##.."
                        , "...##.."
                        , "..####."
                        , "...####" -- repeating variant (end)        [11]
                        , "...##.."
                        , "...##.."
                        , "...##.."
                        , "...##.."
                        , "..####." -- repeating variant (start)      [6]
                        , "....#.." -- initial height from cave floor [5]
                        , "....#.."
                        , "....#.."
                        , "....#.."
                        , "...####"
                        , "#######"
                        ]
                actual = getPatterns render
                expected = [(Offset 6, PatternLength 6)]
            in actual @?= expected

        , testCase "Finite offset, non-monotonically changing sequence" $ 
            let p =
                  [ "cat"
                  , "ca"
                  , "c"
                  , "a"
                  , "aa"
                  , "aaa"
                  , "ac"
                  , "ab"
                  , "aba"
                  , "abaa"
                  , "abaaa"
                  , "abac"
                  , "abab"
                  , "ababa"
                  , "ababaa"
                  , "ababaaa"
                  , "ababac"
                  , "ababab"
                  ]
                actual =
                  makeDfb p
                    >>= Just . head . getPatterns . NE.toList . diffSequence
                expected = Just (Offset 7, PatternLength 5)
            in actual @?= expected
        ]

    , testGroup "diff" $ 
        [ testCase "Two empty strings" $ 
            diff "" "" @?= ("", "")

        , testCase "Left empty string" $ 
            diff "dog" "" @?= ("dog", "")

        , testCase "Right empty string" $
            diff "" "dog" @?= ("", "dog")

        , testCase "Both different and non-empty" $
            diff "dog" "cat" @?= ("dog", "cat")

        , testCase "Both same and non-empty" $
            diff "dog" "dog" @?= ("", "")

        , testCase "One adds onto the other at the front" $
            diff "dog" "catdog" @?= ("dog", "catdog")

        , testCase "One adds onto the other at the end" $
            diff "catdog" "cat" @?= ("dog", "")

        , testCase "Same front portion, ends are different" $
            diff "catdog" "catsheep" @?= ("dog", "sheep")
        ]

    , testGroup "diffSequence" $
        [ testCase "No elements" $
            let actual = makeDfb [[]] >>= Just . diffSequence
                expected = Nothing :: Maybe (NonEmpty (Diff String))
            in actual @?= expected

        , testCase "Single element" $
            let actual = makeDfb ["dog"] >>= Just . diffSequence
                expected = Nothing
            in actual @?= expected

        , testCase "Two elements, no change" $
            let actual = makeDfb ["dog", "dog"] >>= Just . diffSequence
                expected = Just $ ("", "") :| []
            in actual @?= expected

        , testCase "Three elements, no change" $
            let actual = makeDfb ["dog", "dog", "dog"] >>= Just . diffSequence
                expected = Just $ ("", "") :| [("", "")]
            in actual @?= expected

        , testCase "Four elements, no change" $
            let p = ["dog", "dog", "dog", "dog"]
                actual = makeDfb p >>= Just . diffSequence
                expected = Just $ ("", "") :| [("", ""), ("", "")]
            in actual @?= expected

        , testCase "Two elements, suffix change" $
            let p = ["dog", "dogcat"]
                actual = makeDfb p >>= Just . diffSequence
                expected = Just $ ("", "cat") :| []
            in actual @?= expected

        , testCase "Three elements, suffix changes" $
            let p = ["dog", "dogcat", "dogsheep"]
                actual = makeDfb p >>= Just . diffSequence
                expected = Just $ ("", "cat") :| [("cat", "sheep")]
            in actual @?= expected

        , testCase "No offset, non-monotonically changing sequence" $
            let p =
                  [ ""
                  , "a"
                  , "aa"
                  , "aaa"
                  , "ac"
                  , "ab"
                  , "aba"
                  , "abaa"
                  , "abaaa"
                  , "abac"
                  , "abab"
                  , "ababa"
                  , "ababaa"
                  , "ababaaa"
                  , "ababac"
                  , "ababab"
                  ]
                diffs =
                  ("", "a")
                    :| [ ("", "a")
                      , ("", "a")
                      , ("aa", "c")
                      , ("c", "b")
                      , ("", "a")
                      , ("", "a")
                      , ("", "a")
                      , ("aa", "c")
                      , ("c", "b")
                      , ("", "a")
                      , ("", "a")
                      , ("", "a")
                      , ("aa", "c")
                      , ("c", "b")
                      ]
                actual = makeDfb p >>= Just . diffSequence
                expected = Just diffs
            in actual @?= expected

        , testCase "Finite offset, non-monotonically changing sequence" $
            let p =
                  [ "cat"
                  , "ca"
                  , "c"
                  , "a"
                  , "aa"
                  , "aaa"
                  , "ac"
                  , "ab"
                  , "aba"
                  , "abaa"
                  , "abaaa"
                  , "abac"
                  , "abab"
                  , "ababa"
                  , "ababaa"
                  , "ababaaa"
                  , "ababac"
                  , "ababab"
                  ]
                diffs =
                  ("t", "")
                    :| [ ("a", "")
                      , ("c", "a")
                      , ("", "a")
                      , ("", "a")
                      , ("aa", "c")
                      , ("c", "b")
                      , ("", "a")
                      , ("", "a")
                      , ("", "a")
                      , ("aa", "c")
                      , ("c", "b")
                      , ("", "a")
                      , ("", "a")
                      , ("", "a")
                      , ("aa", "c")
                      , ("c", "b")
                      ]
                actual = makeDfb p >>= Just . diffSequence
                expected = Just diffs
            in actual @?= expected
        ]
    ]

