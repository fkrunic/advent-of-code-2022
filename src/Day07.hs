{-# LANGUAGE OverloadedStrings #-}

module Day07 where

import Data.Char (isAlphaNum)
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.List (init)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

type Assignment = (Int, Int)

data Line
  = ChangeDirectoryCommand Text
  | ListCommand
  | ListedDirectory Text
  | ListedFile Integer Text
  deriving (Show, Eq)

data FileSystem
  = File Text Integer
  | Directory Text [FileSystem]
  deriving (Show, Eq)

data SystemState = SystemState
  { ssFileSystem :: FileSystem,
    ssCurrentPath :: [Text]
  }

------------------------------------------------------------------------

sc :: Parser ()
sc = L.space space1 empty empty

lexer :: Parser a -> Parser a
lexer = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Integer
integer = lexer L.decimal

anyWord :: Parser Text
anyWord = takeWhile1P Nothing (/= '\n')

pChangeDirectoryCMD :: Parser Line
pChangeDirectoryCMD =
  ChangeDirectoryCommand <$> (symbol "$" *> symbol "cd" *> anyWord)

pListCMD :: Parser Line
pListCMD =
  symbol "$" *> symbol "ls" $> ListCommand

pCMD :: Parser Line
pCMD = try pListCMD <|> pChangeDirectoryCMD

pListedDir :: Parser Line
pListedDir =
  ListedDirectory <$> (symbol "dir" *> lexer anyWord)

pListedFile :: Parser Line
pListedFile =
  ListedFile <$> integer <*> lexer anyWord

pLine :: Parser Line
pLine =
  choice
    [ pCMD <* optional (char '\n'),
      pListedDir <* optional (char '\n'),
      pListedFile <* optional (char '\n')
    ]

------------------------------------------------------------------------

update :: Line -> SystemState -> SystemState
update (ChangeDirectoryCommand "/") ss = ss {ssCurrentPath = ["/"]}
update (ChangeDirectoryCommand "..") ss = ss {ssCurrentPath = init (ssCurrentPath ss)}
update (ChangeDirectoryCommand child) ss = ss {ssCurrentPath = ssCurrentPath ss ++ [child]}
update ListCommand ss = ss
update (ListedDirectory child) ss =
  ss {ssFileSystem = addDirectory child (ssCurrentPath ss) (ssFileSystem ss)}
update (ListedFile size child) ss =
  ss {ssFileSystem = addFile size child (ssCurrentPath ss) (ssFileSystem ss)}

addDirectory :: Text -> [Text] -> FileSystem -> FileSystem
addDirectory _ _ f@(File _ _) = f
addDirectory _ [] fs = fs
addDirectory child (p : []) d@(Directory path children)
  | path == p = Directory path (Directory child [] : children)
  | otherwise = d
addDirectory child (p : deeper) d@(Directory path children)
  | path == p = Directory path (map (addDirectory child deeper) children)
  | otherwise = d

addFile :: Integer -> Text -> [Text] -> FileSystem -> FileSystem
addFile _ _ _ f@(File _ _) = f
addFile _ _ [] fs = fs
addFile size child (p : []) d@(Directory path children)
  | path == p = Directory path (File child size : children)
  | otherwise = d
addFile size child (p : deeper) d@(Directory path children)
  | path == p = Directory path (map (addFile size child deeper) children)
  | otherwise = d

------------------------------------------------------------------------

puzzleInput :: Text
puzzleInput =
  T.intercalate
    "\n"
    [ "$ cd /",
      "$ ls",
      "dir a",
      "14848514 b.txt",
      "8504156 c.dat",
      "dir d",
      "$ cd a",
      "$ ls",
      "dir e",
      "29116 f",
      "2557 g",
      "62596 h.lst",
      "$ cd e",
      "$ ls",
      "584 i",
      "$ cd ..",
      "$ cd ..",
      "$ cd d",
      "$ ls",
      "4060174 j",
      "8033020 d.log",
      "5626152 d.ext",
      "7214296 k"
    ]