module Day08 (
  part1Solution,
  part2Solution,
) where

import Data.Char (digitToInt)
import Data.List (unfoldr)
import Data.Map (Map)
import Data.Map.Strict qualified as M

data TreeView = TreeView
  { treeHeight :: Height
  , treeNorthView :: [Height]
  , treeSouthView :: [Height]
  , treeWestView :: [Height]
  , treeEastView :: [Height]
  }
  deriving (Show, Eq)

newtype XCoordinate = XCoordinate Int deriving (Show, Eq, Ord)

newtype YCoordinate = YCoordinate Int deriving (Show, Eq, Ord)

type Coordinate = (XCoordinate, YCoordinate)

newtype Height = Height Int deriving (Show, Eq, Ord)

type Grid a = Map Coordinate a

isOnExterior :: TreeView -> Bool
isOnExterior t =
  null (treeNorthView t)
    || null (treeSouthView t)
    || null (treeWestView t)
    || null (treeEastView t)

isVisibleFromView :: Height -> [Height] -> Bool
isVisibleFromView height view =
  height > maximum view

isVisible :: TreeView -> Bool
isVisible t =
  isOnExterior t
    || visibleFromNorth
    || visibleFromSouth
    || visibleFromWest
    || visibleFromEast
 where
  height = treeHeight t
  visibleFromNorth = isVisibleFromView height (treeNorthView t)
  visibleFromSouth = isVisibleFromView height (treeSouthView t)
  visibleFromWest = isVisibleFromView height (treeWestView t)
  visibleFromEast = isVisibleFromView height (treeEastView t)

viewingDistance :: Height -> [Height] -> Int
viewingDistance height hs =
  if null rest
    then length view
    else length view + 1
 where
  (view, rest) = span (< height) hs

scenicScore :: TreeView -> Int
scenicScore t = vdNorth * vdSouth * vdWest * vdEast
 where
  vdNorth = viewingDistance (treeHeight t) (treeNorthView t)
  vdSouth = viewingDistance (treeHeight t) (treeSouthView t)
  vdWest = viewingDistance (treeHeight t) (treeWestView t)
  vdEast = viewingDistance (treeHeight t) (treeEastView t)

toGrid :: [[a]] -> Grid a
toGrid xxs =
  M.fromList
    [ ((XCoordinate xCoord, YCoordinate yCoord), v)
    | (yCoord, ys) <- zip [1 ..] xxs
    , (xCoord, v) <- zip [1 ..] ys
    ]

moveNorth :: Coordinate -> Coordinate
moveNorth (xCoord, YCoordinate y) = (xCoord, YCoordinate (y - 1))

moveSouth :: Coordinate -> Coordinate
moveSouth (xCoord, YCoordinate y) = (xCoord, YCoordinate (y + 1))

moveWest :: Coordinate -> Coordinate
moveWest (XCoordinate x, yCoord) = (XCoordinate (x - 1), yCoord)

moveEast :: Coordinate -> Coordinate
moveEast (XCoordinate x, yCoord) = (XCoordinate (x + 1), yCoord)

generateDirectionalView ::
  Grid a ->
  (Coordinate -> Coordinate) ->
  Coordinate ->
  [a]
generateDirectionalView grid mover = tail . unfoldr scanner
 where
  scanner coord = do
    v <- M.lookup coord grid
    Just (v, mover coord)

generateTreeView :: Grid Height -> Coordinate -> Height -> TreeView
generateTreeView grid coord height =
  TreeView
    { treeHeight = height
    , treeNorthView = generateDirectionalView grid moveNorth coord
    , treeSouthView = generateDirectionalView grid moveSouth coord
    , treeWestView = generateDirectionalView grid moveWest coord
    , treeEastView = generateDirectionalView grid moveEast coord
    }

markVisibility :: Grid Height -> [(Coordinate, Bool)]
markVisibility grid = map getVisibility $ M.toList grid
 where
  getVisibility (coord, height) =
    (coord, isVisible $ generateTreeView grid coord height)

totalVisible :: Grid Height -> Int
totalVisible = length . filter snd . markVisibility

toHeights :: String -> [[Height]]
toHeights = map (map (Height . digitToInt)) . lines

getBestScenicScore :: Grid Height -> Int
getBestScenicScore grid = findScenery grid
 where
  findScenery =
    maximum
      . map (scenicScore . uncurry (generateTreeView grid))
      . M.toList

part1Solution :: String -> Int
part1Solution = totalVisible . toGrid . toHeights

part2Solution :: String -> Int
part2Solution = getBestScenicScore . toGrid . toHeights
