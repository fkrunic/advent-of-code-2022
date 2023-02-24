module Problems.Day07 (
  part1Solution,
  part2Solution,
) where

import Data.Either (fromRight)
import Data.Functor (($>))
import Data.List (sort)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (
  MonadParsec (takeWhile1P, try),
  Parsec,
  choice,
  empty,
  optional,
  runParser,
  some,
  (<|>),
 )
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

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
  { ssFileSystem :: FileSystem
  , ssCurrentPath :: [Text]
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------

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
    [ pCMD <* optional (char '\n')
    , pListedDir <* optional (char '\n')
    , pListedFile <* optional (char '\n')
    ]

--------------------------------------------------------------------------------

update :: Line -> SystemState -> SystemState
update (ChangeDirectoryCommand "/") ss = ss{ssCurrentPath = ["/"]}
update (ChangeDirectoryCommand "..") ss =
  ss{ssCurrentPath = init (ssCurrentPath ss)}
update (ChangeDirectoryCommand child) ss =
  ss{ssCurrentPath = ssCurrentPath ss ++ [child]}
update ListCommand ss = ss
update (ListedDirectory child) ss =
  ss{ssFileSystem = addDirectory child (ssCurrentPath ss) (ssFileSystem ss)}
update (ListedFile size child) ss =
  ss{ssFileSystem = addFile size child (ssCurrentPath ss) (ssFileSystem ss)}

addDirectory :: Text -> [Text] -> FileSystem -> FileSystem
addDirectory _ _ f@(File _ _) = f
addDirectory _ [] fs = fs
addDirectory child [p] d@(Directory path children)
  | path == p = Directory path (Directory child [] : children)
  | otherwise = d
addDirectory child (p : deeper) d@(Directory path children)
  | path == p = Directory path (map (addDirectory child deeper) children)
  | otherwise = d

addFile :: Integer -> Text -> [Text] -> FileSystem -> FileSystem
addFile _ _ _ f@(File _ _) = f
addFile _ _ [] fs = fs
addFile size child [p] d@(Directory path children)
  | path == p = Directory path (File child size : children)
  | otherwise = d
addFile size child (p : deeper) d@(Directory path children)
  | path == p = Directory path (map (addFile size child deeper) children)
  | otherwise = d

totalSize :: FileSystem -> Integer
totalSize (File _ size) = size
totalSize (Directory _ children) = sum (map totalSize children)

tally :: FileSystem -> Integer
tally (File _ _) = 0
tally d@(Directory _ children) =
  if t1 <= 100000
    then t1 + sum (map tally children)
    else sum (map tally children)
 where
  t1 = totalSize d

findSizes :: FileSystem -> [Integer]
findSizes (File _ _) = []
findSizes d@(Directory _ children) =
  totalSize d : concatMap findSizes children

targetSize :: FileSystem -> Integer
targetSize fs = finder fs
 where
  currentSize = totalSize fs
  unusedSpace = 70000000 - currentSize
  needToDelete = 30000000 - unusedSpace
  finder = head . dropWhile (< needToDelete) . sort . findSizes

--------------------------------------------------------------------------------

parse :: Text -> [Line]
parse = fromRight [] . runParser (some pLine) ""

initialSystemState :: SystemState
initialSystemState = SystemState (Directory "/" []) ["/"]

buildFileSystemFromCMDs :: Text -> FileSystem
buildFileSystemFromCMDs =
  ssFileSystem
    . foldr update initialSystemState
    . reverse
    . parse

part1Solution :: Text -> Integer
part1Solution = tally . buildFileSystemFromCMDs

part2Solution :: Text -> Integer
part2Solution = targetSize . buildFileSystemFromCMDs
