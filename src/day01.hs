module Day01 where

import System.IO
import Data.List (sort)

first :: IO ()
second :: IO ()

main :: IO ()
main = do
    first
    second

parseInput :: IO ([Int], [Int])
parseInput = do
    input <- readFile "input/day01.txt"
    let res = unzip (map ((\[x, y] -> (read x :: Int, read y :: Int)) . words) (lines input))
    return res

first = do
    (lista, listb) <- parseInput

    let sorteda = sort lista
    let sortedb = sort listb

    -- let diff = sum $ map (\(a,b)->abs (a-b)) (zip sorteda sortedb)
    let diff = sum $ zipWith (\ a b -> abs (a-b) ) sorteda sortedb

    print diff

second = do
    (lista, listb) <- parseInput

    let total = sum $ map (\ x -> x * length (filter (==x) listb)) lista

    print total
