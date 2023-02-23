module Test.Day17Spec where

import Test.Hspec
import Day17

spec :: SpecWith ()
spec = do
  describe "Day 17 Tests" $ do
    it "Parsing wind direction" $ do
      let actual = parse ">>><<"
          expected = [East, East, East, West, West]
      actual `shouldBe` expected

    it 