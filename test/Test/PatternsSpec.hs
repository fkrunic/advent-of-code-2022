module Test.PatternsSpec (spec) where

import Data.List
import Data.Text qualified as T
import Test.Hspec
import Utilities.Patterns

spec :: SpecWith ()
spec = do
  describe "Pattern Tests" $ do
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
      getPatterns "asfi3[12][12][12][12]" `shouldBe`
        [ (Offset 5, PatternLength 4)
        , (Offset 5, PatternLength 8)
        , (Offset 9, PatternLength 4)
        , (Offset 13, PatternLength 4)
        ]

    it "Cave Rendering" $ do
      let render = reverse $ T.lines $
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
          expected = [ (Offset 6, PatternLength 6) ]
      actual `shouldBe` expected
               