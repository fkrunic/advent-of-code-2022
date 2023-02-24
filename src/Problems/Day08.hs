module Problems.Day08 (
  part1Solution,
  part2Solution,
) where

import Data.Char (digitToInt)
import Data.Map.Strict qualified as M

import Utilities.Grids hiding (toGrid)

data TreeView = TreeView
  { treeHeight :: Height
  , treeNorthView :: [Height]
  , treeSouthView :: [Height]
  , treeWestView :: [Height]
  , treeEastView :: [Height]
  }
  deriving (Show, Eq)

newtype Height = Height Int deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------

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

generateTreeView :: Grid Height -> Coordinate -> Height -> TreeView
generateTreeView grid coord height =
  TreeView
    { treeHeight = height
    , treeNorthView = map snd $ generateDirectionalView grid moveNorth coord
    , treeSouthView = map snd $ generateDirectionalView grid moveSouth coord
    , treeWestView = map snd $ generateDirectionalView grid moveWest coord
    , treeEastView = map snd $ generateDirectionalView grid moveEast coord
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
