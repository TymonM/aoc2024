module Day02 where

import System.IO

first :: IO ()
second :: IO ()

main :: IO ()
main = do
    first
    second

parseReports :: IO [[Int]]
parseReports = do
    input <- readFile "input/day02.txt"
    let res = map (map read . words) (lines input)
    return res

isAscending :: [Int] -> Bool
isAscending v = all (uncurry (<)) (zip v (tail v))
isDescending :: [Int] -> Bool
isDescending v = all (uncurry (>)) (zip v (tail v))
isMonotonic :: [Int] -> Bool
isMonotonic v = isAscending v || isDescending v
isWellSpaced :: [Int] -> Bool
isWellSpaced v = all (\(a,b) -> 1 <= abs(a-b) && abs(a-b) <= 3) (zip v (tail v))
isSafeReport :: [Int] -> Bool
isSafeReport v = isMonotonic v && isWellSpaced v

first = do
    reports <- parseReports

    print (length $ filter isSafeReport reports)

-- this is cooler than it looks, the extra case of x=length v takes care of 'null' alteration
alterReport :: [Int] -> [[Int]]
alterReport v = map (\x -> take x v ++ drop (x+1) v) [0..(length v)]

second = do
    reports <- parseReports

    let alteredReports = map alterReport reports

    print (length $ filter (any isSafeReport) alteredReports)