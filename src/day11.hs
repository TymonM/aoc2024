module Day11 where

import System.IO
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Int (Int64)

first :: IO ()
second :: IO ()

main :: IO ()
main = do
    first
    second

blink :: Int -> Int -> Int
blink cnt n
    | cnt == 0 = 1
    | even $ length digits = blink (cnt-1) (read $ take (length digits `div` 2) digits) + blink (cnt-1) (read $ drop (length digits `div` 2) digits)
    | n == 0 = blink (cnt-1) 1
    | otherwise = blink (cnt-1) (2024*n)
    where
        digits = show n

first = do
    stones <- map read . words <$> readFile "input/day11.txt"

    print (sum $ map (blink 25) stones)

memoBlink :: Int -> Int64 -> Map (Int, Int64) Int64 -> (Map (Int, Int64) Int64, Int64)
memoBlink cnt n memo
    | cnt == 0 = (memo, 1)
    | Map.member (cnt, n) memo = (memo, fromJust $ Map.lookup (cnt, n) memo)
    | even $ length digits = do
        let left = memoBlink (cnt-1) (read $ take (length digits `div` 2) digits) memo
        let right = memoBlink (cnt-1) (read $ drop (length digits `div` 2) digits) (fst left)
        let res = snd left + snd right
        let finalMemo = Map.insert (cnt, n) res (fst right)
        (finalMemo, res)
    | n == 0 = do
        let res = memoBlink (cnt-1) 1 memo
        let finalMemo = Map.insert (cnt, n) (snd res) (fst res)
        (finalMemo, snd res)
    | otherwise = do
        let res = memoBlink (cnt-1) (2024*n) memo
        let finalMemo = Map.insert (cnt, n) (snd res) (fst res)
        (finalMemo, snd res)
    where
        digits = show n

second = do
    stones <- map read . words <$> readFile "input/day11.txt"

    let (mp, _) = foldr (\n (m, _) -> memoBlink 75 n m) (Map.empty, 0) stones

    print (sum $ map (snd . flip (memoBlink 75) mp) stones)