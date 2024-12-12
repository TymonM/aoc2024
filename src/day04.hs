module Day04 where

import System.IO
import Data.List (isPrefixOf, tails)

first :: IO ()
second :: IO ()

main :: IO ()
main = do
    first
    second

readGrid :: IO [String]
readGrid = lines <$> readFile "input/day04.txt"

countSubstring :: String -> [String] -> Int
countSubstring key grid = sum $ map (length . filter (isPrefixOf key) . tails) grid

rotate45 :: [String] -> [String]
rotate45 grid = do
    let height = length grid
    let width = length $ head grid
    map (\xysum -> map (\x -> grid !! x !! (xysum-x)) ([max (0) (xysum-width+1)..min (xysum) (height-1)])) ([0..(height + width - 2)])

rotate90 :: [String] -> [String]
rotate90 grid = do
    let height = length grid
    let width = length $ head grid
    map (\y -> map (\x -> grid !! x !! (width - y - 1)) [0..height-1]) [0..width-1]

first = do
    grid <- readGrid

    let diags = rotate45 grid
    let once = rotate90 grid
    let oncediags = rotate45 once
    let twice = rotate90 once
    let twicediags = rotate45 twice
    let thrice = rotate90 twice
    let thricediags = rotate45 thrice

    let variants = [grid,diags,once,oncediags,twice,twicediags,thrice,thricediags]
    let total = sum $ map (countSubstring "XMAS") variants
    
    print total

hasGoodNeighbourhood :: [String] -> Int -> Int -> Bool
hasGoodNeighbourhood grid row col = do
    let corners = [(grid !! (row-1) !! (col-1)), (grid !! (row-1) !! (col+1)), (grid !! (row+1) !! (col-1)), (grid !! (row+1) !! (col+1))]
    let mcnt = length $ filter (=='M') corners
    let scnt = length $ filter (=='S') corners
    (mcnt == 2) && (scnt == 2) && ((corners !! 0 > corners !! 1) == (corners !! 2 > corners !! 3))

second = do
    grid <- readGrid
    let height = length grid
    let width = length $ head grid

    let allA = map (\row -> filter (\col -> (grid !! row !! col == 'A' && hasGoodNeighbourhood grid row col)) [1..width-2]) [1..height-2]

    print (sum $ map length allA)