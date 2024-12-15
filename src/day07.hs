module Day07 where
import Data.Char (isDigit)
import Data.Int (Int64)
import Data.List (isSuffixOf)

first :: IO ()
second :: IO ()

main :: IO ()
main = do
    first
    second

data Equation = Equation {
    terms :: [Int64],
    target :: Int64
} deriving Show

parseEquation :: String -> Equation
parseEquation s = do
    let target = read $ takeWhile isDigit s
    let terms = map read $ tail $ words s
    -- we reverse the terms because operations are applied left to right
    -- but recursion (see `satisfiable`) is right to left
    Equation {terms = reverse terms, target = target}

readEquations :: IO [Equation]
readEquations = do
    input <- readFile "input/day07.txt"
    return (map parseEquation $ lines input)

satisfiable :: [Int64 -> Int64 -> Int64] -> Equation -> Bool
satisfiable _ (Equation {terms = [t], target = target}) = target == t
satisfiable options (Equation {terms = (x:tail), target = target})
    | target < 0 = False
    | otherwise = any (satisfiable options . \f -> Equation {terms = tail, target = f target x}) options
satisfiable _ _ = False

divide :: Int64 -> Int64 -> Int64
divide p q
    | mod p q == 0 = div p q
    | otherwise = -1

first = do
    equations <- readEquations
    let options = [(-),divide]
    print (sum $ map target $ filter (satisfiable options) equations)

deconcatenate :: Int64 -> Int64 -> Int64
deconcatenate p q
    | (p /= q) && substr `isSuffixOf` s = read $ take (length s - length substr) s
    | otherwise = -1
    where
        s = show p
        substr = show q

second = do
    equations <- readEquations
    let options = [(-),divide,deconcatenate]
    print (sum $ map target $ filter (satisfiable options) equations)