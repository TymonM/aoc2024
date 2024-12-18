module Day09 where

import System.IO
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Char (digitToInt)
import Data.List (partition, find)
import Data.Set (Set)
import qualified Data.Set as Set

first :: IO ()
second :: IO ()

main :: IO ()
main = do
    first
    second

readDiskChunks :: IO [Maybe Int]
readDiskChunks = do
    input <- readFile "input/day09.txt"
    return (concatMap (\(sz, i) -> replicate sz (if odd i then Nothing else Just (i `div` 2))) (zip (map digitToInt input) [0..]))

fragment :: Int -> [Maybe Int] -> [Maybe Int] -> [Int]
fragment num (d:ds) (b:bs)
    | num == 0 = []
    | isNothing b = fragment num (d:ds) bs
    | isNothing d = fromJust b : fragment (num-1) ds bs
    | isJust d = fromJust d : fragment (num-1) ds (b:bs)


first = do
    disk <- readDiskChunks
    let backdisk = reverse disk

    let sz = length $ filter isJust disk
    let final = fragment sz disk backdisk

    print (sum $ zipWith (*) final [0..])

data Space = Space {start :: Int, size :: Int} deriving Show
data File = File {value :: Int, space :: Space} deriving Show

readDiskFiles :: IO ([File], [Space])
readDiskFiles = do
    input <- readFile "input/day09.txt"
    let converted = map digitToInt input
    let withIndex = zip3 [0..] converted (0 : scanl1 (+) converted)
    let (files', spaces') = partition (\(x,_,_) -> even x) withIndex
    let files = map (\(i, sz, start) -> File {
        value = i `div` 2,
        space = Space {
            start = start,
            size = sz
            }
        }) files'
    let spaces = map (\(_, sz, start) -> Space {
        start = start, size = sz
        }) spaces'
    return (files, spaces)

defragment :: ([File], [Space]) -> Set Int -> [File]
defragment (f:fs, s:ss) taken
    | (start (space f) < start s) && not (Set.member (value f) taken) = f : defragment (fs, s:ss) taken
    | (start (space f) < start s) && Set.member (value f) taken = defragment (fs, s:ss) taken
    | isJust lastFittingFile = do
        let f' = fromJust lastFittingFile
        let newFile = File {
            value = value f',
            space = Space {
                start = start s,
                size = size $ space f'
            }
        }
        let newSpace = Space {
            start = start s + size (space f'),
            size = size s - size (space f')
        }
        let newTaken = Set.insert (value f') taken
        newFile : defragment (f:fs, newSpace:ss) newTaken
    | otherwise = defragment (f:fs, ss) taken
    where lastFittingFile = find (\file -> not (Set.member (value file) taken) && (size (space file) <= size s)) (reverse (f:fs))
defragment _ _ = []

checksumContribution :: File -> Int
checksumContribution f = value f * sum (take (size $ space f) [start $ space f ..])

second = do
    disk <- readDiskFiles

    let final = defragment disk Set.empty

    print (sum $ map checksumContribution final)
