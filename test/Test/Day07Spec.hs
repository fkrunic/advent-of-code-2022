module Test.Day07Spec (spec) where

import Data.Text (Text, intercalate)
import Problems.Day07 (part1Solution, part2Solution)
import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec =
  testGroup "Day 7 Solution" $
    [ testCase "Part 1" $
        part1Solution puzzleInput @?= 95437

    , testCase "Part 2" $ do
        part2Solution puzzleInput @?= 24933642
    ]

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
