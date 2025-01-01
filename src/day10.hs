module Day10 where

import System.IO
import Data.Char (digitToInt)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import AocUtils (cartesianProduct)

firstAndSecond :: IO ()

main :: IO ()
main = do
    firstAndSecond

readGrid :: IO [[Int]]
readGrid = map (map digitToInt) . lines <$> readFile "input/day10.txt"

neighboursEqualTo :: (Int, Int) -> [[Int]] -> Int -> [(Int, Int)]
neighboursEqualTo (x, y) grid targetValue = filter (\nbr -> inBounds nbr && matchTarget nbr) allNeighbours
    where
        height = length grid
        width = length $ head grid
        allNeighbours = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
        inBounds (xx, yy) = xx >= 0 && yy >= 0 && xx < width && yy < height
        matchTarget (xx, yy) = grid !! yy !! xx == targetValue

countNines :: (Int, Int) -> [[Int]] -> Map (Int, Int) (Int, Set (Int, Int)) -> Int -> Map (Int, Int) (Int, Set (Int, Int))
countNines (x, y) grid mp cur
    | cur == (-1) = mp
    | x == width = countNines (0, y+1) grid mp cur
    | y == height = countNines (0, 0) grid mp (cur-1)
    | cur == 9 && (grid !! y !! x == 9) = do
        let newMp = Map.insert (x,y) (1, Set.singleton (x,y)) mp
        countNines (x+1, y) grid newMp cur
    | cur == 9 = countNines (x+1, y) grid mp cur
    | grid !! y !! x == cur = do
        let newSet = foldr (\nbr st -> Set.union (snd $ fromJust $ Map.lookup nbr mp) st) Set.empty (neighboursEqualTo (x,y) grid (cur+1))
        let newCnt = sum $ map (\nbr -> fst $ fromJust $ Map.lookup nbr mp) (neighboursEqualTo (x,y) grid (cur+1))
        let newMp = Map.insert (x,y) (newCnt, newSet) mp
        countNines (x+1, y) grid newMp cur
    | otherwise = countNines (x+1, y) grid mp cur
    where
        height = length grid
        width = length $ head grid

firstAndSecond = do
    grid <- readGrid

    let reachableNines = countNines (0,0) grid Map.empty 9

    let width = length $ head grid
    let height = length grid
    let zeroIndexes = filter (\(x,y) -> grid !! y !! x == 0) (cartesianProduct [0..width-1] [0..height-1])

    print (sum $ map (Set.size . snd . fromJust . (`Map.lookup` reachableNines)) zeroIndexes)
    print (sum $ map (fst . fromJust . (`Map.lookup` reachableNines)) zeroIndexes)
