module Day05 where

import System.IO
import Data.List.Split (splitWhen)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

first :: IO ()
second ::IO ()

main :: IO ()
main = do
    first
    second

readConstraint :: String -> (Int, Int)
readConstraint s = do
    let nums = map read (splitWhen (=='|') s)
    (nums !! 0, nums !! 1)

readConstraintsAndUpdates :: IO ([(Int, Int)], [[Int]])
readConstraintsAndUpdates = do
    input <- readFile "input/day05.txt"
    let sections = splitWhen (==[]) (lines input)
    let constraints = map readConstraint (sections !! 0)
    let updates = map (map read . splitWhen (==',')) (sections !! 1)
    return (constraints, updates)

sufficesConstraint :: [Int] -> (Int, Int) -> Bool
sufficesConstraint ls constraint = do
    let firstIndex = fromMaybe (-1) $ elemIndex (fst constraint) ls
    let secondIndex = fromMaybe 1000 $ elemIndex (snd constraint) ls
    firstIndex < secondIndex

suffices :: [(Int, Int)] -> [Int] -> Bool
suffices constraints ls = all (sufficesConstraint ls) constraints

middleElement :: [Int] -> Int
middleElement arr = arr !! div (length arr-1) 2

first = do
    (constraints, updates) <- readConstraintsAndUpdates

    let printedUpdates = filter (suffices constraints) updates
    let total = sum $ map middleElement printedUpdates

    print total

fixUpdate :: [(Int, Int)] -> [Int] -> [Int]
fixUpdate constraints ls
    | suffices constraints ls = ls
    | otherwise = do
        let bad = head $ filter (not . sufficesConstraint ls) constraints
        let newArr = map (\x -> if x == fst bad then snd bad else if x == snd bad then fst bad else x) ls
        fixUpdate constraints newArr

-- My first slow slow solution... almost one second
-- Hopefully there aren't too many more to come........
second = do
    (constraints, updates) <- readConstraintsAndUpdates

    let unprintedUpdates = filter (not . suffices constraints) updates
    let fixed = map (fixUpdate constraints) unprintedUpdates
    let total = sum $ map middleElement fixed

    print total
