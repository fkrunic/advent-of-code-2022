module Test.PatternsSpec (spec) where

import Data.List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Test.Hspec
import Utilities.Patterns

spec :: SpecWith ()
spec = do
  describe "Pattern Tests" $ do
    describe "getPatterns" $ do
      it "''" $ do
        getPatterns "" `shouldBe` []

      it "'a'" $ do
        getPatterns "a" `shouldBe` []

      it "'ab'" $ do
        getPatterns "ab" `shouldBe` []

      it "'aa'" $ do
        getPatterns "aa" `shouldBe` [(Offset 0, PatternLength 1)]

      it "'aaa'" $ do
        getPatterns "aaa"
          `shouldBe` [ (Offset 0, PatternLength 1)
                     , (Offset 1, PatternLength 1)
                     ]

      it "'9smxundu39[abc][abc]'" $ do
        chop 5 "[abc][abc]" `shouldBe` ["[abc]", "[abc]"]
        group (chop 5 "[abc][abc]") `shouldBe` [["[abc]", "[abc]"]]
        splitAt 10 "9smxundu39[abc][abc]" `shouldBe` ("9smxundu39", "[abc][abc]")
        hasPattern (Offset 10) (PatternLength 5) "9smxundu39[abc][abc]" `shouldBe` True
        getPatterns "9smxundu39[abc][abc]"
          `shouldBe` [(Offset 10, PatternLength 5)]

      it "'asfi3[12][12][12][12]'" $ do
        getPatterns "asfi3[12][12][12][12]"
          `shouldBe` [ (Offset 5, PatternLength 4)
                     , (Offset 5, PatternLength 8)
                     , (Offset 9, PatternLength 4)
                     , (Offset 13, PatternLength 4)
                     ]

      it "Cave Rendering" $ do
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
        actual `shouldBe` expected

      it "Finite offset, non-monotonically changing sequence" $ do
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
        actual `shouldBe` expected

    describe "diff" $ do
      it "Two empty strings" $ do
        diff "" "" `shouldBe` ("", "")

      it "Left empty string" $ do
        diff "dog" "" `shouldBe` ("dog", "")

      it "Right empty string" $ do
        diff "" "dog" `shouldBe` ("", "dog")

      it "Both different and non-empty" $ do
        diff "dog" "cat" `shouldBe` ("dog", "cat")

      it "Both same and non-empty" $ do
        diff "dog" "dog" `shouldBe` ("", "")

      it "One adds onto the other at the front" $ do
        diff "dog" "catdog" `shouldBe` ("dog", "catdog")

      it "One adds onto the other at the end" $ do
        diff "catdog" "cat" `shouldBe` ("dog", "")

      it "Same front portion, ends are different" $ do
        diff "catdog" "catsheep" `shouldBe` ("dog", "sheep")

    describe "diffSequence" $ do
      it "No elements" $ do
        let actual = makeDfb [[]] >>= Just . diffSequence
            expected = Nothing :: Maybe (NonEmpty (Diff String))
        actual `shouldBe` expected

      it "Single element" $ do
        let actual = makeDfb ["dog"] >>= Just . diffSequence
            expected = Nothing
        actual `shouldBe` expected

      it "Two elements, no change" $ do
        let actual = makeDfb ["dog", "dog"] >>= Just . diffSequence
            expected = Just $ ("", "") :| []
        actual `shouldBe` expected

      it "Three elements, no change" $ do
        let actual = makeDfb ["dog", "dog", "dog"] >>= Just . diffSequence
            expected = Just $ ("", "") :| [("", "")]
        actual `shouldBe` expected

      it "Four elements, no change" $ do
        let p = ["dog", "dog", "dog", "dog"]
            actual = makeDfb p >>= Just . diffSequence
            expected = Just $ ("", "") :| [("", ""), ("", "")]
        actual `shouldBe` expected

      it "Two elements, suffix change" $ do
        let p = ["dog", "dogcat"]
            actual = makeDfb p >>= Just . diffSequence
            expected = Just $ ("", "cat") :| []
        actual `shouldBe` expected

      it "Three elements, suffix changes" $ do
        let p = ["dog", "dogcat", "dogsheep"]
            actual = makeDfb p >>= Just . diffSequence
            expected = Just $ ("", "cat") :| [("cat", "sheep")]
        actual `shouldBe` expected

      it "No offset, non-monotonically changing sequence" $ do
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
        actual `shouldBe` expected

      it "Finite offset, non-monotonically changing sequence" $ do
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
        actual `shouldBe` expected