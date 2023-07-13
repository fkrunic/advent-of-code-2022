module Test.Day12Spec (spec) where

import Data.Either (fromRight)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Problems.Day12 (
  Cell,
  CellType (EndCell, GenericCell, StartCell),
  Grid,
  Height (Height),
  XCoordinate (XCoordinate),
  YCoordinate (YCoordinate),
  getEdges,
  nextMoves,
  pLine,
  toPoints,
  vertices,
 )
import Utilities.Graphs (
  Vertex (Vertex),
  dijkstra,
  dijkstraMultipleSources,
  extractTargetDistance,
 )
import Text.Megaparsec (optional, runParser, some)
import Text.Megaparsec.Char (newline)

import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec =
  testGroup "Day 12 Tests" $
    [ testCase "Parsing Puzzle Input" $
        parser exampleInput @?= parsedCells

    , testCase "Converting Cells to Gridpoints" $
        toPoints parsedCells @?= parsedGrid

    , testGroup "Finding Next Moves" $
        [ testCase "From Start Position" $
            let start = ((XCoordinate 0, YCoordinate 0), (StartCell, Height 0))
                actual = nextMoves start parsedGrid
                expected =
                  [ ((XCoordinate 0, YCoordinate 1), (GenericCell, Height 0)) -- a
                  , ((XCoordinate 1, YCoordinate 0), (GenericCell, Height 0)) -- a
                  ]
            in actual @?= expected

        , testCase "From Close to End" $
            let start = ((XCoordinate 5, YCoordinate 1), (GenericCell, Height 23)) -- x
                actual = nextMoves start parsedGrid
                expected =
                  [ ((XCoordinate 5, YCoordinate 0), (GenericCell, Height 14)) -- o
                  , ((XCoordinate 4, YCoordinate 1), (GenericCell, Height 24)) -- y
                  , ((XCoordinate 6, YCoordinate 1), (GenericCell, Height 23)) -- x
                  ]
            in actual @?= expected

        , testCase "From Top-Right Corner" $
            let start = ((XCoordinate 7, YCoordinate 0), (GenericCell, Height 12)) -- m
                actual = nextMoves start parsedGrid
                expected =
                  [ ((XCoordinate 7, YCoordinate 1), (GenericCell, Height 11)) -- l
                  , ((XCoordinate 6, YCoordinate 0), (GenericCell, Height 13)) -- n
                  ]
            in actual @?= expected

        , testCase "From Spiral" $
            let start = ((XCoordinate 3, YCoordinate 2), (GenericCell, Height 18)) -- s
                actual = nextMoves start parsedGrid
                expected =
                  [ ((XCoordinate 3, YCoordinate 1), (GenericCell, Height 17)) -- r
                  , ((XCoordinate 3, YCoordinate 3), (GenericCell, Height 19)) -- t
                  , ((XCoordinate 2, YCoordinate 2), (GenericCell, Height 2)) -- c
                  ]
            in actual @?= expected
        ]

    , testGroup "Puzzle Solutions" $
        [ testCase "Example Input - Dijkstra" $
            part1Dijkstra exampleInput @?= 31

        , testCase "Example Input - Dijkstra (Multiple Sources)" $
            part2Dijkstra exampleInput @?= 29

        , testCase "Part 1 Solution - Dijkstra" $
            part1Dijkstra puzzleInput @?= 339

        , testCase "Part 2 Solution - Dijkstra" $
            part2Dijkstra puzzleInput @?= 332
        ]
    ]

part1Dijkstra :: Text -> Int
part1Dijkstra t = targetDistance
 where
  grid = toPoints $ parser t
  startVertex =
    Vertex $ head $ filter ((== StartCell) . fst . snd) $ M.assocs grid
  dMap = dijkstra startVertex (vertices grid) (getEdges grid)
  targetDistance =
    maybe (-1) fromIntegral $
      extractTargetDistance ((== EndCell) . fst . snd) dMap

part2Dijkstra :: Text -> Int
part2Dijkstra t = targetDistance
 where
  grid = toPoints $ parser t
  sources = map Vertex $ filter ((== Height 0) . snd . snd) $ M.assocs grid
  dMap =
    dijkstraMultipleSources
      sources
      (vertices grid)
      (getEdges grid)
  targetDistance =
    maybe (-1) fromIntegral $
      extractTargetDistance ((== EndCell) . fst . snd) dMap

parser :: Text -> [[Cell]]
parser = fromRight [] . runParser (some (pLine <* optional newline)) ""

parsedGrid :: Grid
parsedGrid =
  M.fromList
    [ ((XCoordinate 0, YCoordinate 0), (StartCell, Height 0)) -- S
    , ((XCoordinate 1, YCoordinate 0), (GenericCell, Height 0)) -- a
    , ((XCoordinate 2, YCoordinate 0), (GenericCell, Height 1)) -- b
    , ((XCoordinate 3, YCoordinate 0), (GenericCell, Height 16)) -- q
    , ((XCoordinate 4, YCoordinate 0), (GenericCell, Height 15)) -- p
    , ((XCoordinate 5, YCoordinate 0), (GenericCell, Height 14)) -- o
    , ((XCoordinate 6, YCoordinate 0), (GenericCell, Height 13)) -- n
    , ((XCoordinate 7, YCoordinate 0), (GenericCell, Height 12)) -- m
    , ((XCoordinate 0, YCoordinate 1), (GenericCell, Height 0)) -- a
    , ((XCoordinate 1, YCoordinate 1), (GenericCell, Height 1)) -- b
    , ((XCoordinate 2, YCoordinate 1), (GenericCell, Height 2)) -- c
    , ((XCoordinate 3, YCoordinate 1), (GenericCell, Height 17)) -- r
    , ((XCoordinate 4, YCoordinate 1), (GenericCell, Height 24)) -- y
    , ((XCoordinate 5, YCoordinate 1), (GenericCell, Height 23)) -- x
    , ((XCoordinate 6, YCoordinate 1), (GenericCell, Height 23)) -- x
    , ((XCoordinate 7, YCoordinate 1), (GenericCell, Height 11)) -- l
    , ((XCoordinate 0, YCoordinate 2), (GenericCell, Height 0)) -- a
    , ((XCoordinate 1, YCoordinate 2), (GenericCell, Height 2)) -- c
    , ((XCoordinate 2, YCoordinate 2), (GenericCell, Height 2)) -- c
    , ((XCoordinate 3, YCoordinate 2), (GenericCell, Height 18)) -- s
    , ((XCoordinate 4, YCoordinate 2), (GenericCell, Height 25)) -- z
    , ((XCoordinate 5, YCoordinate 2), (EndCell, Height 25)) -- E
    , ((XCoordinate 6, YCoordinate 2), (GenericCell, Height 23)) -- x
    , ((XCoordinate 7, YCoordinate 2), (GenericCell, Height 10)) -- k
    , ((XCoordinate 0, YCoordinate 3), (GenericCell, Height 0)) -- a
    , ((XCoordinate 1, YCoordinate 3), (GenericCell, Height 2)) -- c
    , ((XCoordinate 2, YCoordinate 3), (GenericCell, Height 2)) -- c
    , ((XCoordinate 3, YCoordinate 3), (GenericCell, Height 19)) -- t
    , ((XCoordinate 4, YCoordinate 3), (GenericCell, Height 20)) -- u
    , ((XCoordinate 5, YCoordinate 3), (GenericCell, Height 21)) -- v
    , ((XCoordinate 6, YCoordinate 3), (GenericCell, Height 22)) -- w
    , ((XCoordinate 7, YCoordinate 3), (GenericCell, Height 9)) -- j
    , ((XCoordinate 0, YCoordinate 4), (GenericCell, Height 0)) -- a
    , ((XCoordinate 1, YCoordinate 4), (GenericCell, Height 1)) -- b
    , ((XCoordinate 2, YCoordinate 4), (GenericCell, Height 3)) -- d
    , ((XCoordinate 3, YCoordinate 4), (GenericCell, Height 4)) -- e
    , ((XCoordinate 4, YCoordinate 4), (GenericCell, Height 5)) -- f
    , ((XCoordinate 5, YCoordinate 4), (GenericCell, Height 6)) -- g
    , ((XCoordinate 6, YCoordinate 4), (GenericCell, Height 7)) -- h
    , ((XCoordinate 7, YCoordinate 4), (GenericCell, Height 8)) -- i
    ]

parsedCells :: [[Cell]]
parsedCells =
  [
    [ (StartCell, Height 0) -- S
    , (GenericCell, Height 0) -- a
    , (GenericCell, Height 1) -- b
    , (GenericCell, Height 16) -- q
    , (GenericCell, Height 15) -- p
    , (GenericCell, Height 14) -- o
    , (GenericCell, Height 13) -- n
    , (GenericCell, Height 12) -- m
    ]
  ,
    [ (GenericCell, Height 0) -- a
    , (GenericCell, Height 1) -- b
    , (GenericCell, Height 2) -- c
    , (GenericCell, Height 17) -- r
    , (GenericCell, Height 24) -- y
    , (GenericCell, Height 23) -- x
    , (GenericCell, Height 23) -- x
    , (GenericCell, Height 11) -- l
    ]
  ,
    [ (GenericCell, Height 0) -- a
    , (GenericCell, Height 2) -- c
    , (GenericCell, Height 2) -- c
    , (GenericCell, Height 18) -- s
    , (GenericCell, Height 25) -- z
    , (EndCell, Height 25) -- E
    , (GenericCell, Height 23) -- x
    , (GenericCell, Height 10) -- k
    ]
  ,
    [ (GenericCell, Height 0) -- a
    , (GenericCell, Height 2) -- c
    , (GenericCell, Height 2) -- c
    , (GenericCell, Height 19) -- t
    , (GenericCell, Height 20) -- u
    , (GenericCell, Height 21) -- v
    , (GenericCell, Height 22) -- w
    , (GenericCell, Height 9) -- v
    ]
  ,
    [ (GenericCell, Height 0) -- a
    , (GenericCell, Height 1) -- b
    , (GenericCell, Height 3) -- d
    , (GenericCell, Height 4) -- e
    , (GenericCell, Height 5) -- f
    , (GenericCell, Height 6) -- g
    , (GenericCell, Height 7) -- h
    , (GenericCell, Height 8) -- i
    ]
  ]

exampleInput :: Text
exampleInput =
  T.intercalate
    "\n"
    [ "Sabqponm"
    , "abcryxxl"
    , "accszExk"
    , "acctuvwj"
    , "abdefghi"
    ]

puzzleInput :: Text
puzzleInput =
  T.intercalate
    "\n"
    [ "abcccccccaaaaaccccaaaaaaaccccccccccccccccccccccccccccccccccccaaaaa"
    , "abaacccaaaaaaccccccaaaaaaaaaaaaaccccccccccccccccccccccccccccaaaaaa"
    , "abaacccaaaaaaaccccaaaaaaaaaaaaaacccccccccccccaacccccccccccccaaaaaa"
    , "abaacccccaaaaaacaaaaaaaaaaaaaaaacccccccccccccaacccccccccccccacacaa"
    , "abaccccccaaccaacaaaaaaaaaacccaacccccccccccccaaacccccccccccccccccaa"
    , "abcccccccaaaacccaaaaaaaaacccccccccccccaaacccaaacccccccccccccccccaa"
    , "abccccccccaaaccccccccaaaacccccccccccccaaaaacaaaccacacccccccccccccc"
    , "abccccccccaaacaaacccccaaacccccccccccccaaaaaaajjjjjkkkcccccaacccccc"
    , "abcccccaaaaaaaaaacccccaaccccccccccciiiiiijjjjjjjjjkkkcaaaaaacccccc"
    , "abcccccaaaaaaaaacccccccccccccccccciiiiiiijjjjjjjrrkkkkaaaaaaaacccc"
    , "abcccccccaaaaaccccccccccccccccccciiiiiiiijjjjrrrrrppkkkaaaaaaacccc"
    , "abcccaaccaaaaaacccccccccccaacaaciiiiqqqqqrrrrrrrrpppkkkaaaaaaacccc"
    , "abccaaaaaaaaaaaaccccacccccaaaaaciiiqqqqqqrrrrrruuppppkkaaaaacccccc"
    , "abcccaaaaaaacaaaacaaacccccaaaaaahiiqqqqtttrrruuuuupppkkaaaaacccccc"
    , "abcaaaaaaaccccaaaaaaacccccaaaaaahhqqqtttttuuuuuuuuuppkkkccaacccccc"
    , "abcaaaaaaaaccccaaaaaacccccaaaaaahhqqqtttttuuuuxxuuuppkklcccccccccc"
    , "abcaaaaaaaacaaaaaaaaaaacccccaaachhhqqtttxxxuuxxyyuuppllllccccccccc"
    , "abcccaaacaccaaaaaaaaaaaccccccccchhhqqtttxxxxxxxyuupppplllccccccccc"
    , "abaacaacccccaaaaaaaaaaaccccccccchhhqqtttxxxxxxyyvvvpppplllcccccccc"
    , "abaacccccccccaaaaaaacccccccccccchhhpppttxxxxxyyyvvvvpqqqlllccccccc"
    , "SbaaccccccaaaaaaaaaaccccccccccchhhppptttxxxEzzyyyyvvvqqqlllccccccc"
    , "abaaaaccccaaaaaaaaacccccccccccchhhpppsssxxxyyyyyyyyvvvqqqlllcccccc"
    , "abaaaacccccaaaaaaaacccccccccccgggpppsssxxyyyyyyyyyvvvvqqqlllcccccc"
    , "abaaacccaaaacaaaaaaaccccccccccgggpppsswwwwwwyyyvvvvvvqqqllllcccccc"
    , "abaaccccaaaacaaccaaaacccccccccgggppssswwwwwwyyywvvvvqqqqmmmccccccc"
    , "abaaccccaaaacaaccaaaaccaaaccccggpppssssswwswwyywvqqqqqqmmmmccccccc"
    , "abcccccccaaacccccaaacccaaacaccgggpppssssssswwwwwwrqqmmmmmccccccccc"
    , "abcccccccccccccccccccaacaaaaacgggppooosssssrwwwwrrrmmmmmcccccccccc"
    , "abcccccccccccccccccccaaaaaaaacggggoooooooorrrwwwrrnmmmdddccaaccccc"
    , "abaccccccccccccaacccccaaaaaccccggggoooooooorrrrrrrnmmddddcaaaccccc"
    , "abaccccccccaaaaaaccccccaaaaaccccggfffffooooorrrrrnnndddddaaaaccccc"
    , "abaacccccccaaaaaacccccaaaaaacccccffffffffoonrrrrrnnndddaaaaaaacccc"
    , "abaaccccccccaaaaaaaccacaaaacccccccccffffffonnnnnnnndddaaaaaaaacccc"
    , "abccccccccccaaaaaaaaaaaaaaaccccccccccccfffennnnnnnddddccaaaccccccc"
    , "abcccccccccaaaaaaacaaaaaaaaaacccccccccccffeennnnnedddccccaaccccccc"
    , "abcccccccccaaaaaaccaaaaaaaaaaaccccccccccaeeeeeeeeeedcccccccccccccc"
    , "abccccccccccccaaaccaaaaaaaaaaaccccccccccaaaeeeeeeeecccccccccccccaa"
    , "abcccccccaaccccccccaaaaaaaacccccccccccccaaaceeeeecccccccccccccccaa"
    , "abaaccaaaaaaccccccccaaaaaaaacccccccccccccaccccaaacccccccccccaaacaa"
    , "abaaccaaaaacccccaaaaaaaaaaacccccccccccccccccccccacccccccccccaaaaaa"
    , "abaccaaaaaaaaccaaaaaaaaaaaaaacccccccccccccccccccccccccccccccaaaaaa"
    ]
