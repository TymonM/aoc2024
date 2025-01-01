module AocUtils where

cartesianProduct :: [Int] -> [Int] -> [(Int, Int)]
cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]