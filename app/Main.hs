module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["1"] -> Day01.main
        ["2"] -> Day02.main
        ["3"] -> Day03.main
        ["4"] -> Day04.main
        ["5"] -> Day05.main
        ["6"] -> Day06.main
        ["7"] -> Day07.main
        ["8"] -> Day08.main
        ["9"] -> Day09.main
        ["10"] -> Day10.main
        ["11"] -> Day11.main
        [] -> do
            putStrLn "Day 01"
            Day01.main
            putStrLn "\nDay 02"
            Day02.main
            putStrLn "\nDay 03"
            Day03.main
            putStrLn "\nDay 04"
            Day04.main
            putStrLn "\nDay 05"
            Day05.main
            putStrLn "\nDay 06"
            Day06.main
            putStrLn "\nDay 07"
            Day07.main
            putStrLn "\nDay 08"
            Day08.main
            putStrLn "\nDay 09"
            Day09.main
            putStrLn "\nDay 10"
            Day10.main
            putStrLn "\nDay 11"
            Day11.main
        _     -> do
            putStrLn "Usage: aoc2024runner [daynumber]"
            exitFailure