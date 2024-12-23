{-# LANGUAGE OverloadedStrings #-}

module Day13 where

import Data.Attoparsec.Text
import Data.Text (pack)
import Control.Applicative
import Data.Either (fromRight)

first :: IO ()
second :: IO ()

main :: IO ()
main = do
    first
    second

data Machine = Machine {
    buttonA :: (Int, Int),
    buttonB :: (Int, Int),
    target :: (Int, Int)
} deriving (Show)

coordParser :: Parser (Int, Int)
coordParser = do
    _ <- char 'X'
    _ <- char '+' <|> char '='
    x <- decimal
    _ <- string ", Y+" <|> string ", Y="
    y <- decimal
    return (x, y)

machineParser :: Parser Machine
machineParser = do
    _ <- string "Button A: "
    a <- coordParser
    _ <- endOfLine
    _ <- string "Button B: "
    b <- coordParser
    _ <- endOfLine
    _ <- string "Prize: "
    t <- coordParser
    _ <- optional endOfLine
    _ <- endOfLine <|> endOfInput
    return $ Machine a b t

machinesParser :: Parser [Machine]
machinesParser = many' machineParser

readMachines :: IO (Either String [Machine])
readMachines = do
    input <- readFile "input/day13.txt"
    return $ parseOnly machinesParser (pack input)

cartesianProduct :: [Int] -> [Int] -> [(Int, Int)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]

solveMachineBruteForce :: Machine -> Int
solveMachineBruteForce Machine{buttonA=(ax,ay), buttonB=(bx,by), target=(tx, ty)} = do
    let valid = filter (\(a, b) -> a*ax + b*bx == tx && a*ay + b*by == ty) (cartesianProduct [0..100] [0..100])
    let min' = foldr (\(a, b) acc -> if acc == 0 then 3*a+b else min acc (3*a+b)) 0 valid
    min'

first = do
    machines <- fromRight [] <$> readMachines
    print (sum $ map solveMachineBruteForce machines)

adjustTarget :: Machine -> Machine
adjustTarget machine = machine{target = addToCoord 10000000000000 (target machine)}
    where addToCoord n (x, y) = (x+n, y+n)

onSpan :: (Int, Int) -> (Int, Int) -> Bool
onSpan (ax, ay) (bx, by) = fromIntegral ax / fromIntegral bx == fromIntegral ay / fromIntegral by

modinv :: Int -> Int -> Int
modinv x m = head $ filter (\y -> (x*y) `mod` m == 1) [1..m-1]

-- see 'Chicken McNugget Theorem'
-- but the gcd thing is my own idea yay
solveLinear :: Int -> Int -> Int -> Int
solveLinear a b t
    | t `mod` g /= 0 = 0
    | g /= 1 = solveLinear (div a g) (div b g) (div t g)
    | t < a*b = do
        let acnt = (t * modinv a b) `mod` b
        if t >= acnt * a then do
            let bcnt = (t-acnt*a) `div` b
            3*acnt + bcnt
        else 0
    | otherwise = (div t a*b) * (min (b*3) a) + solveLinear a b (t `mod` a*b)
    where g = gcd a b

solveMachineFast :: Machine -> Int
solveMachineFast Machine{buttonA=(ax,ay), buttonB=(bx,by), target=(tx, ty)}
    | not (onSpan (ax, ay) (bx, by)) = do
        if (tx * by - bx * ty) `mod` (ax * by - bx * ay) == 0 && (ax * ty - tx * ay) `mod` (ax * by - bx * ay) == 0 then do
            let a = div (tx * by - bx * ty) (ax * by - bx * ay)
            let b = div (ax * ty - tx * ay) (ax * by - bx * ay)
            if a >= 0 && b >= 0 then 3*a+b else 0
        else 0
    | onSpan (ax, ay) (tx, ty) = solveLinear ax bx tx
    | otherwise = 0

second = do
    machines <- map adjustTarget . fromRight [] <$> readMachines
    print (sum $ map solveMachineFast machines)