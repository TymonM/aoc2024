module Day12 where

import System.IO
import Data.Set (Set)
import qualified Data.Set as Set
import AocUtils (cartesianProduct)

first :: IO ()
second :: IO ()

main :: IO ()
main = do
    first
    second

readGrid :: IO [[Char]]
readGrid = lines <$> readFile "input/day12.txt"

isMatchingNeighbour :: [[Char]] -> (Int, Int) -> (Int, Int) -> Bool
isMatchingNeighbour grid (x,y) (dx,dy)
    | inBounds (x,y) && inBounds (x+dx,y+dy) = grid !! y !! x == grid !! (y+dy) !! (x+dx)
    | otherwise = False
    where
        height = length grid
        width = length $ head grid
        inBounds (xx, yy) = xx >= 0 && yy >= 0 && xx < width && yy < height

neighboursEqualTo :: (Int, Int) -> [[Char]] -> [(Int, Int)]
neighboursEqualTo (x, y) grid = map addPair $ filter (isMatchingNeighbour grid (x,y)) allDirections
    where
        allDirections = [(1,0),(0,1),(-1,0),(0,-1)]
        addPair (xx, yy) = (x+xx, y+yy)

getAreaAndPerimeter :: [[Char]] -> Set (Int, Int) -> (Int, Int) -> ((Int, Int), Set (Int, Int))
getAreaAndPerimeter grid st pos
    | Set.member pos st = ((0, 0), st)
    | otherwise = foldr (\pos' (info, st') -> mergeTuple (info, st') (getAreaAndPerimeter grid st' pos') ) ((1, 4-length neighbours), Set.insert pos st) neighbours
    where
        mergeTuple ((area, perimeter), _) ((area', perimeter'), st') = ((area+area', perimeter+perimeter'), st')
        neighbours = neighboursEqualTo pos grid

merge :: [[Char]] -> (Int, Int) -> (Int, Set (Int, Int)) -> (Int, Set (Int, Int))
merge grid pos (subtotal, st) = do
    let ((area, perimeter), st') = getAreaAndPerimeter grid st pos
    (subtotal + (area*perimeter), st')

first = do
    grid <- readGrid

    let height = length grid
    let width = length $ head grid
    let (total, _) = foldr (merge grid) (0, Set.empty) (cartesianProduct [0..width-1] [0..height-1])

    print total

getAreaAndSides :: [[Char]] -> Set (Int, Int) -> (Int, Int) -> ((Int, Int), Set (Int, Int))
getAreaAndSides grid st pos
    | Set.member pos st = ((0, 0), st)
    | otherwise = foldr (\pos' (info, st') -> mergeTuple (info, st') (getAreaAndSides grid st' pos') ) ((1, sidecnt), Set.insert pos st) neighbours
    where
        mergeTuple ((area, sides), _) ((area', sides'), st') = ((area+area', sides+sides'), st')
        neighbours = neighboursEqualTo pos grid
        rightSide = not (isMatchingNeighbour grid pos (1, 0)) && not (isMatchingNeighbour grid pos (0, -1) && not (isMatchingNeighbour grid pos (1, -1)))
        leftSide = not (isMatchingNeighbour grid pos (-1, 0)) && not (isMatchingNeighbour grid pos (0, -1) && not (isMatchingNeighbour grid pos (-1, -1)))
        upSide = not (isMatchingNeighbour grid pos (0, -1)) && not (isMatchingNeighbour grid pos (1, 0) && not (isMatchingNeighbour grid pos (1, -1)))
        downSide = not (isMatchingNeighbour grid pos (0, 1)) && not (isMatchingNeighbour grid pos (1, 0) && not (isMatchingNeighbour grid pos (1, 1)))
        sidecnt = length $ filter id [rightSide, leftSide, upSide, downSide]

merge2 :: [[Char]] -> (Int, Int) -> (Int, Set (Int, Int)) -> (Int, Set (Int, Int))
merge2 grid pos (subtotal, st) = do
    let ((area, sides), st') = getAreaAndSides grid st pos
    (subtotal + (area*sides), st')

second = do
    grid <- readGrid

    let height = length grid
    let width = length $ head grid
    let (total, _) = foldr (merge2 grid) (0, Set.empty) (cartesianProduct [0..width-1] [0..height-1])

    print total
