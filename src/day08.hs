module Day08 where

import System.IO
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (tails)

first :: IO ()
second :: IO ()

main :: IO ()
main = do
    first
    second

newtype Coord = Coord (Int,Int) deriving (Show, Eq, Ord)

data Grid = Grid {
    antennas :: Map Char [Coord],
    width :: Int,
    height :: Int
} deriving Show

insertAntenna :: (Char, Coord) -> Grid -> Grid
insertAntenna (c,p) grid = grid { antennas = Map.insertWith (++) c [p] (antennas grid) }

indexToCoord :: Int -> Int -> Coord
indexToCoord w i = Coord (mod i w, div i w)

readGrid :: IO Grid
readGrid = do
    raw <- readFile "input/day08.txt"
    let rawlines = lines raw
    let width = length $ head rawlines
    let height = length rawlines
    let oneline = concat rawlines
    let defaultGrid = Grid {
                antennas = Map.empty,
                width = width,
                height = height
            }

    return (foldr (insertAntenna . (\(c, p) -> (c, indexToCoord width p))) defaultGrid (filter ((/= '.') . fst) (zip oneline [0..])))

allPairs :: [Coord] -> [(Coord, Coord)]
allPairs ls = [(x,y) | (x:ys) <- tails ls, y <- ys]

insertAntinodes :: Int -> Int -> (Coord, Coord) -> Set Coord -> Set Coord
insertAntinodes from to (Coord (x1, y1), Coord (x2, y2)) set = do
    let dx = x2 - x1
    let dy = y2 - y1
    let addNode1 = foldr (Set.insert . (\d -> Coord(x1-d*dx,y1-d*dy))) set [from..to]
    let addNode2 = foldr (Set.insert . (\d -> Coord(x2+d*dx,y2+d*dy))) addNode1 [from..to]
    addNode2

isInGrid :: Grid -> Coord -> Bool
isInGrid grid (Coord(x, y)) = x >= 0 && y >= 0 && x < width grid && y < height grid

first = do
    grid <- readGrid

    let antinodes = foldr (insertAntinodes 1 1) Set.empty (concatMap allPairs (Map.elems $ antennas grid))
    let antinodesInGrid = filter (isInGrid grid) $ Set.elems antinodes

    print (length antinodesInGrid)

second = do
    grid <- readGrid

    let maxDimension = max (width grid) (height grid)
    let antinodes = foldr (insertAntinodes 0 maxDimension) Set.empty (concatMap allPairs (Map.elems $ antennas grid))
    let antinodesInGrid = filter (isInGrid grid) $ Set.elems antinodes

    print (length antinodesInGrid)