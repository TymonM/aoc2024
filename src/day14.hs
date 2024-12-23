{-# LANGUAGE OverloadedStrings #-}

module Day14 where

import System.IO
import Data.Attoparsec.Text
import Data.Text (pack)
import Control.Applicative
import Data.Either (fromRight)
import Data.List

first :: IO ()
second :: IO ()

main :: IO ()
main = do
    first
    second

data Robot = Robot {
    p :: (Int, Int),
    v :: (Int, Int)
} deriving (Show)

coordParser :: Parser (Int, Int)
coordParser = do
    x <- signed decimal
    _ <- char ','
    y <- signed decimal
    _ <- optional $ char ' '
    _ <- optional endOfLine
    return (x, y)

robotParser :: Parser Robot
robotParser = do
    _ <- string "p="
    p <- coordParser
    _ <- string "v="
    v <- coordParser
    return $ Robot p v

robotsParser :: Parser [Robot]
robotsParser = many' robotParser

readRobots :: IO [Robot]
readRobots = do
    input <- readFile "input/day14.txt"
    return $ fromRight [] $ parseOnly robotsParser (pack input)

simulate :: (Int, Int) -> Int -> Robot -> Robot
simulate (w, h) steps robot
    | steps <= 0 = robot
    | otherwise = simulate (w,h) (steps-1) robot{p=next}
    where
        next = (((fst (p robot) + fst (v robot)) `mod` w + w) `mod` w, ((snd (p robot) + snd (v robot)) `mod` h + h) `mod` h)

first = do
    robots <- readRobots
    let (w, h) = (101, 103)
    let finalRobots = map (p . simulate (w, h) 100) robots
    let ul = length $ filter (\(x, y) -> x < w `div` 2 && y < h `div` 2) finalRobots
    let ur = length $ filter (\(x, y) -> x > w `div` 2 && y < h `div` 2) finalRobots
    let dl = length $ filter (\(x, y) -> x < w `div` 2 && y > h `div` 2) finalRobots
    let dr = length $ filter (\(x, y) -> x > w `div` 2 && y > h `div` 2) finalRobots
    print $ ul * ur * dl * dr

entropy :: [(Int, Int)] -> Int
entropy points = sum $ map singleEntropy points
    where
        manhattanDistance (a, b) (c, d) = abs (a-c) + abs (b-d)
        singleEntropy point = minimum $ map (manhattanDistance point) (points \\ [point])
        -- NOTE: minimum could be done faster by storing robots in 2d array and running bfs from each one
        -- but haskell is annoying for that stuff

simulateAllAndCalculateEntropy :: [Robot] -> (Int, Int) -> [Int] -> ([Int], [Robot])
simulateAllAndCalculateEntropy robots (w, h) entropies = do
    let robots' = map (simulate (w, h) 1) robots
    let entropies' = (entropy $ map p robots') : entropies
    (entropies', robots')

-- runs in 54 seconds :sweat: :phew:
second = do
    --todo
    robots <- readRobots
    let (w, h) = (101, 103)

    let (entropies, _) = foldl (\(entropies, robots') _ -> simulateAllAndCalculateEntropy robots' (w,h) entropies) ([], robots) [1..w*h-1]
    let (_e, mn) = minimum $ zip (reverse entropies) [1..]

    print mn

    -- to see the christmas tree:
    -- let funny = map (p . simulate (w, h) mn) robots
    -- let grid = map (\y -> map (\x -> if elem (x, y) funny then '#' else '.') [0..w-1]) [0..h-1]
    -- mapM_ putStrLn grid