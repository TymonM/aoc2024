module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified Day01
import qualified Day02
import qualified Day03

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["1"] -> Day01.main
        ["2"] -> Day02.main
        ["3"] -> Day03.main
        [] -> do
            putStrLn "Day 01"
            Day01.main
            putStrLn "\nDay 02"
            Day02.main
            putStrLn "\nDay 03"
            Day03.main
        _     -> do
            putStrLn "Usage: aoc-2024 [daynumber]"
            exitFailure