module Day03 where

import System.IO
import Text.Regex.PCRE

first :: IO ()
second :: IO ()

main :: IO ()
main = do
    first
    second

allStringMatches :: String -> String -> [String]
allStringMatches s regex = do
    getAllTextMatches (s =~ regex :: AllTextMatches [] String)

doMul :: String -> Int
doMul s = do
    let matches = allStringMatches s "[0-9][0-9]*"
    product $ map read matches

first = do
    instructions <- readFile "input/day03.txt"

    let matches = allStringMatches instructions "mul\\([0-9][0-9]?[0-9]?,[0-9][0-9]?[0-9]?\\)"
    let total = sum $ map doMul matches

    print total

second = do
    instructions <- readFile "input/day03.txt"
    
    let wrapped = "do()" ++ (concat . lines) instructions ++ "don't()"
    let enabledBlocks = allStringMatches wrapped "do\\(\\).*?don't\\(\\)"
    let matches = allStringMatches (concat enabledBlocks) "mul\\([0-9][0-9]?[0-9]?,[0-9][0-9]?[0-9]?\\)"

    let total = sum $ map doMul matches

    print total
