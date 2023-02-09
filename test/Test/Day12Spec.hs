module Test.Day12Spec (spec) where

import Test.Hspec

import Data.Either (fromRight)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Day12
import Text.Megaparsec
import Text.Megaparsec.Char

spec :: SpecWith ()
spec =
  describe "Day 12 Tests" $ do
    it "Parsing Puzzle Input" $ do
      parser exampleInput `shouldBe` parsedCells

    it "Converting Cells to Gridpoints" $ do
      toPoints parsedCells `shouldBe` parsedGrid

    describe "Finding Next Moves" $ do
      it "From Start Position" $ do
        let start = ((XCoordinate 0, YCoordinate 0), (StartCell, Height 0))
            actual = nextMoves start parsedGrid
            expected =
              [ ((XCoordinate 0, YCoordinate 1), (GenericCell, Height 0)) -- a
              , ((XCoordinate 1, YCoordinate 0), (GenericCell, Height 0)) -- a
              ]
        actual `shouldBe` expected

      it "From Close to End" $ do
        let start = ((XCoordinate 5, YCoordinate 1), (GenericCell, Height 23)) -- x
            actual = nextMoves start parsedGrid
            expected =
              [ ((XCoordinate 5, YCoordinate 0), (GenericCell, Height 14)) -- o
              , ((XCoordinate 4, YCoordinate 1), (GenericCell, Height 24)) -- y
              , ((XCoordinate 6, YCoordinate 1), (GenericCell, Height 23)) -- x
              ]
        actual `shouldBe` expected

      it "From Top-Right Corner" $ do
        let start = ((XCoordinate 7, YCoordinate 0), (GenericCell, Height 12)) -- m
            actual = nextMoves start parsedGrid
            expected =
              [ ((XCoordinate 7, YCoordinate 1), (GenericCell, Height 11)) -- l
              , ((XCoordinate 6, YCoordinate 0), (GenericCell, Height 13)) -- n
              ]
        actual `shouldBe` expected

      it "From Spiral" $ do
        let start = ((XCoordinate 3, YCoordinate 2), (GenericCell, Height 18)) -- s
            actual = nextMoves start parsedGrid
            expected =
              [ ((XCoordinate 3, YCoordinate 1), (GenericCell, Height 17)) -- r
              , ((XCoordinate 3, YCoordinate 3), (GenericCell, Height 19)) -- t
              , ((XCoordinate 2, YCoordinate 2), (GenericCell, Height 2)) -- c
              ]
        actual `shouldBe` expected

    describe "Expanding Paths" $ do
      {-
        Sabqponm
        abcryxxl
        accszExk
        acctuvwj
        abdefghi

        >>v.....
        >Ev.....
        ^<<.....
        ........
        ........

      -}
      it "Correctly terminates a dead-end path" $ do
        let path =
              reverse
                [ ((XCoordinate 0, YCoordinate 0), (StartCell, Height 0)) -- S
                , ((XCoordinate 1, YCoordinate 0), (GenericCell, Height 0)) -- a
                , ((XCoordinate 2, YCoordinate 0), (GenericCell, Height 1)) -- b
                , ((XCoordinate 2, YCoordinate 1), (GenericCell, Height 2)) -- c
                , ((XCoordinate 2, YCoordinate 2), (GenericCell, Height 2)) -- c
                , ((XCoordinate 1, YCoordinate 2), (GenericCell, Height 2)) -- c
                , ((XCoordinate 0, YCoordinate 2), (GenericCell, Height 0)) -- a
                , ((XCoordinate 0, YCoordinate 1), (GenericCell, Height 0)) -- a
                , ((XCoordinate 1, YCoordinate 1), (GenericCell, Height 1)) -- b (end)
                ]
        expandPath path parsedGrid `shouldBe` []

      {-
        Sabqponm
        abcryxxl
        accszExk
        acctuvwj
        abdefghi

        v>N.....
        >^......
        ........
        ........
        ........

      -}
      it "Avoids backtracking on itself" $ do
        let path =
              reverse
                [ ((XCoordinate 0, YCoordinate 0), (StartCell, Height 0)) -- S
                , ((XCoordinate 0, YCoordinate 1), (GenericCell, Height 0)) -- a
                , ((XCoordinate 1, YCoordinate 1), (GenericCell, Height 1)) -- b
                , ((XCoordinate 1, YCoordinate 0), (GenericCell, Height 0)) -- a
                ]
            next = ((XCoordinate 2, YCoordinate 0), (GenericCell, Height 1)) -- b
            expected = [next : path]
        expandPath path parsedGrid `shouldBe` expected

      {-
        Sabqponm
        abcryxxl
        accszExk
        acctuvwj
        abdefghi

        >v......
        NEN.....
        .M......
        ........
        ........

      -}
      it "Can generate multiple paths forward" $ do
        let path =
              reverse
                [ ((XCoordinate 0, YCoordinate 0), (StartCell, Height 0)) -- S
                , ((XCoordinate 1, YCoordinate 0), (GenericCell, Height 0)) -- a
                , ((XCoordinate 1, YCoordinate 1), (GenericCell, Height 1)) -- b
                ]
            downMove = ((XCoordinate 1, YCoordinate 2), (GenericCell, Height 2)) -- c
            leftMove = ((XCoordinate 0, YCoordinate 1), (GenericCell, Height 0)) -- a
            rightMove = ((XCoordinate 2, YCoordinate 1), (GenericCell, Height 2)) -- c
            expected =
              [ downMove : path
              , leftMove : path
              , rightMove : path
              ]
        expandPath path parsedGrid `shouldBe` expected


      {-
        Sabqponm
        abcryxxl
        accszExk
        acctuvwj
        abdefghi

        v..v<<<<
        v..vv<<^
        v..v>E^^
        v..>>>^^
        >>>>>>>^

      -}
      it "Can find the end point of the path" $ do
        let path = reverse
              [ ((XCoordinate 0, YCoordinate 0), (StartCell, Height 0)) -- S
              , ((XCoordinate 0, YCoordinate 1), (GenericCell, Height 0)) -- a
              , ((XCoordinate 0, YCoordinate 2), (GenericCell, Height 0)) -- a
              , ((XCoordinate 0, YCoordinate 3), (GenericCell, Height 0)) -- a
              , ((XCoordinate 0, YCoordinate 4), (GenericCell, Height 0)) -- a
              , ((XCoordinate 1, YCoordinate 4), (GenericCell, Height 1)) -- b
              , ((XCoordinate 2, YCoordinate 4), (GenericCell, Height 3)) -- d
              , ((XCoordinate 3, YCoordinate 4), (GenericCell, Height 4)) -- e
              , ((XCoordinate 4, YCoordinate 4), (GenericCell, Height 5)) -- f
              , ((XCoordinate 5, YCoordinate 4), (GenericCell, Height 6)) -- g
              , ((XCoordinate 6, YCoordinate 4), (GenericCell, Height 7)) -- h
              , ((XCoordinate 7, YCoordinate 4), (GenericCell, Height 8)) -- i  
              , ((XCoordinate 7, YCoordinate 3), (GenericCell, Height 9)) -- j  
              , ((XCoordinate 7, YCoordinate 2), (GenericCell, Height 10)) -- k  
              , ((XCoordinate 7, YCoordinate 1), (GenericCell, Height 11)) -- l  
              , ((XCoordinate 7, YCoordinate 0), (GenericCell, Height 12)) -- m      
              , ((XCoordinate 6, YCoordinate 0), (GenericCell, Height 13)) -- n
              , ((XCoordinate 5, YCoordinate 0), (GenericCell, Height 14)) -- o
              , ((XCoordinate 4, YCoordinate 0), (GenericCell, Height 15)) -- p
              , ((XCoordinate 3, YCoordinate 0), (GenericCell, Height 16)) -- q
              , ((XCoordinate 3, YCoordinate 1), (GenericCell, Height 17)) -- r
              , ((XCoordinate 3, YCoordinate 2), (GenericCell, Height 18)) -- s 
              , ((XCoordinate 3, YCoordinate 3), (GenericCell, Height 19)) -- t
              , ((XCoordinate 4, YCoordinate 3), (GenericCell, Height 20)) -- u
              , ((XCoordinate 5, YCoordinate 3), (GenericCell, Height 21)) -- v
              , ((XCoordinate 6, YCoordinate 3), (GenericCell, Height 22)) -- w 
              , ((XCoordinate 6, YCoordinate 2), (GenericCell, Height 23)) -- x  
              , ((XCoordinate 6, YCoordinate 1), (GenericCell, Height 23)) -- x
              , ((XCoordinate 5, YCoordinate 1), (GenericCell, Height 23)) -- x
              , ((XCoordinate 4, YCoordinate 1), (GenericCell, Height 24)) -- y
              , ((XCoordinate 4, YCoordinate 2), (GenericCell, Height 25)) -- z                                                                                
              ]
            end = ((XCoordinate 5, YCoordinate 2), (EndCell, Height 25)) -- E
            expected = [ end : path ]
        expandPath path parsedGrid `shouldBe` expected

    {-
               (4)     (2)     (3)     (1)
      Saaaa   >>>>v   v....   >>v..   v.>>v
      azaza   ....v   v....   ..v..   v.^.v 
      aaaaE   ....v   >>>>>   ..>>>   >>^.v

    -}

    describe "Finding Paths" $ do
      it "Finding paths on the simpler grid" $ do
        let startingPaths = 
              [ [ ((XCoordinate 0, YCoordinate 0), (StartCell, Height 0)) -- S 
                ] 
              ]
            firstPath = reverse
              [ ((XCoordinate 0, YCoordinate 0), (StartCell, Height 0)) -- S 
              , ((XCoordinate 0, YCoordinate 1), (GenericCell, Height 0)) -- a 
              , ((XCoordinate 0, YCoordinate 2), (GenericCell, Height 0)) -- a 
              , ((XCoordinate 1, YCoordinate 2), (GenericCell, Height 0)) -- a 
              , ((XCoordinate 2, YCoordinate 2), (GenericCell, Height 0)) -- a 
              , ((XCoordinate 2, YCoordinate 1), (GenericCell, Height 0)) -- a 
              , ((XCoordinate 2, YCoordinate 0), (GenericCell, Height 0)) -- a 
              , ((XCoordinate 3, YCoordinate 0), (GenericCell, Height 0)) -- a 
              , ((XCoordinate 4, YCoordinate 0), (GenericCell, Height 0)) -- a 
              , ((XCoordinate 4, YCoordinate 1), (GenericCell, Height 0)) -- a 
              , ((XCoordinate 4, YCoordinate 2), (EndCell, Height 1)) -- E
              ]
            secondPath = reverse
              [ ((XCoordinate 0, YCoordinate 0), (StartCell, Height 0)) -- S 
              , ((XCoordinate 0, YCoordinate 1), (GenericCell, Height 0)) -- a
              , ((XCoordinate 0, YCoordinate 2), (GenericCell, Height 0)) -- a
              , ((XCoordinate 1, YCoordinate 2), (GenericCell, Height 0)) -- a
              , ((XCoordinate 2, YCoordinate 2), (GenericCell, Height 0)) -- a
              , ((XCoordinate 3, YCoordinate 2), (GenericCell, Height 0)) -- a
              , ((XCoordinate 4, YCoordinate 2), (EndCell, Height 1)) -- a
              ]
            thirdPath = reverse
              [ ((XCoordinate 0, YCoordinate 0), (StartCell, Height 0)) -- S 
              , ((XCoordinate 1, YCoordinate 0), (GenericCell, Height 0)) -- a 
              , ((XCoordinate 2, YCoordinate 0), (GenericCell, Height 0)) -- a
              , ((XCoordinate 2, YCoordinate 1), (GenericCell, Height 0)) -- a
              , ((XCoordinate 2, YCoordinate 2), (GenericCell, Height 0)) -- a
              , ((XCoordinate 3, YCoordinate 2), (GenericCell, Height 0)) -- a
              , ((XCoordinate 4, YCoordinate 2), (EndCell, Height 1)) -- a
              ]
            fourthPath = reverse
              [ ((XCoordinate 0, YCoordinate 0), (StartCell, Height 0)) -- S 
              , ((XCoordinate 1, YCoordinate 0), (GenericCell, Height 0)) -- a
              , ((XCoordinate 2, YCoordinate 0), (GenericCell, Height 0)) -- a
              , ((XCoordinate 3, YCoordinate 0), (GenericCell, Height 0)) -- a
              , ((XCoordinate 4, YCoordinate 0), (GenericCell, Height 0)) -- a
              , ((XCoordinate 4, YCoordinate 1), (GenericCell, Height 0)) -- a
              , ((XCoordinate 4, YCoordinate 2), (EndCell, Height 1)) -- a
              ]
            expected = [firstPath, secondPath, thirdPath, fourthPath]
        findPaths startingPaths simplerGrid `shouldBe` expected

    describe "Puzzle Solutions" $ do
      it "Example Input" $ do
        part1Solution exampleInput `shouldBe` 31

      it "Part 1 Solution" $ do
        pendingWith "Current implementation does not finish. Running time too long."


part1Solution :: Text -> Int
part1Solution = 
  flip (-) 1
    . minimum 
    . map length 
    . findPaths startPaths 
    . toPoints 
    . parser 
  where
    startPaths = [[((XCoordinate 0, YCoordinate 0), (StartCell, Height 0))]]

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

{-
  Saaaa
  azaza
  aaaaE
-}

simplerGrid :: Grid 
simplerGrid = 
  M.fromList 
    [ ((XCoordinate 0, YCoordinate 0), (StartCell, Height 0)) -- S
    , ((XCoordinate 1, YCoordinate 0), (GenericCell, Height 0)) -- a
    , ((XCoordinate 2, YCoordinate 0), (GenericCell, Height 0)) -- a
    , ((XCoordinate 3, YCoordinate 0), (GenericCell, Height 0)) -- a
    , ((XCoordinate 4, YCoordinate 0), (GenericCell, Height 0)) -- a

    , ((XCoordinate 0, YCoordinate 1), (GenericCell, Height 0)) -- a
    , ((XCoordinate 1, YCoordinate 1), (GenericCell, Height 25)) -- z
    , ((XCoordinate 2, YCoordinate 1), (GenericCell, Height 0)) -- a
    , ((XCoordinate 3, YCoordinate 1), (GenericCell, Height 25)) -- z
    , ((XCoordinate 4, YCoordinate 1), (GenericCell, Height 0)) -- a

    , ((XCoordinate 0, YCoordinate 2), (GenericCell, Height 0)) -- a
    , ((XCoordinate 1, YCoordinate 2), (GenericCell, Height 0)) -- a
    , ((XCoordinate 2, YCoordinate 2), (GenericCell, Height 0)) -- a
    , ((XCoordinate 3, YCoordinate 2), (GenericCell, Height 0)) -- a
    , ((XCoordinate 4, YCoordinate 2), (EndCell, Height 1)) -- E
    ]

puzzleInput :: Text 
puzzleInput = 
  T.intercalate "\n"
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