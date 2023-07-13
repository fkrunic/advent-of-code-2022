module Test.Day07Spec (spec) where

import Data.Text (Text, intercalate)
import Problems.Day07 (part1Solution, part2Solution)
import Test.Hspec (SpecWith, describe, it, shouldBe)

spec :: SpecWith ()
spec =
  describe "Day 7 Solution" $ do
    it "Part 1" $ do
      part1Solution puzzleInput `shouldBe` 95437

    it "Part 2" $ do
      part2Solution puzzleInput `shouldBe` 24933642

puzzleInput :: Text
puzzleInput =
  intercalate
    "\n"
    [ "$ cd /"
    , "$ ls"
    , "dir a"
    , "14848514 b.txt"
    , "8504156 c.dat"
    , "dir d"
    , "$ cd a"
    , "$ ls"
    , "dir e"
    , "29116 f"
    , "2557 g"
    , "62596 h.lst"
    , "$ cd e"
    , "$ ls"
    , "584 i"
    , "$ cd .."
    , "$ cd .."
    , "$ cd d"
    , "$ ls"
    , "4060174 j"
    , "8033020 d.log"
    , "5626152 d.ext"
    , "7214296 k"
    ]
