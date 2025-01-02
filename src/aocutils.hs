module AocUtils where

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]