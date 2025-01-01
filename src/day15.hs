module Day15 where

import System.IO
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Split (splitWhen)
import AocUtils (cartesianProduct)
import Data.Maybe (fromJust)

first :: IO ()
second :: IO ()

main :: IO ()
main = do
    first
    second

data Tile = Space | Box | Wall | LeftBox | RightBox deriving (Show, Eq)

data Grid = Grid {
    tiles :: Map (Int, Int) Tile,
    robotPos :: (Int, Int),
    width :: Int,
    height :: Int
} deriving (Show)

parseTile :: Char -> Tile
pareTile '.' = Space
parseTile '@' = Space
parseTile 'O' = Box
parseTile '#' = Wall
parseTile _ = Space

parseGrid :: [String] -> Grid
parseGrid raw = do
    let width = length $ head raw
    let height = length raw
    let tiles = foldr (\(x, y) mp -> Map.insert (x, y) (parseTile $ raw !! y !! x) mp) Map.empty (cartesianProduct [0..width - 1] [0..height - 1])
    let robotPos = head $ filter (\(x, y) -> raw !! y !! x == '@') (cartesianProduct [0..width - 1] [0..height - 1])
    Grid {tiles = tiles, robotPos = robotPos, width = width, height = height}

parseMove :: Char -> (Int, Int)
parseMove '<' = (-1, 0)
parseMove '^' = (0, -1)
parseMove '>' = (1, 0)
parseMove 'v' = (0, 1)
parseMove _ = (0, 0)

parseMoves :: [String] -> [(Int, Int)]
parseMoves raw = map parseMove $ concat raw

readGridAndMoves :: IO (Grid, [(Int, Int)])
readGridAndMoves = do
    raw <- readFile "input/day15.txt"
    let sections = splitWhen null (lines raw)
    let rawGrid = head sections
    let rawMoves = sections !! 1
    return (parseGrid rawGrid, parseMoves rawMoves)

push :: (Int, Int) -> (Int, Int) -> Grid -> Grid
push (x, y) (dx, dy) grid
    | destTile == Space = do
        let tiles' = Map.insert dest (fromJust $ Map.lookup (x, y) (tiles grid)) (tiles grid)
        let tiles'' = Map.insert (x, y) Space tiles'
        let robotPos' = if (x, y) == robotPos grid then dest else robotPos grid
        grid {tiles = tiles'', robotPos = robotPos'}
    | destTile == Wall = grid
    | destTile == Box = do
        let grid' = push dest (dx, dy) grid
        if fromJust (Map.lookup dest (tiles grid')) == Space then push (x, y) (dx, dy) grid' else grid
    where
        dest = (x+dx, y+dy)
        destTile = fromJust $ Map.lookup dest (tiles grid)

doMove :: (Int, Int) -> Grid -> Grid
doMove mv grid = push (robotPos grid) mv grid

calcGps :: (Int, Int) -> Int
calcGps (x, y) = 100 * y + x

first = do
    (grid, moves) <- readGridAndMoves
    let w = width grid
    let h = height grid

    let finalGrid = foldl (flip doMove) grid moves

    -- print $ robotPos finalGrid
    -- mapM_ (\y -> putStrLn (map (\x -> (toEnum . (13 +) . fromEnum . head . show . fromJust . Map.lookup (x, y)) (tiles finalGrid)) [0..w-1])) [0..h-1]

    let gpsSum = sum $ map calcGps $ filter (\(x, y) -> fromJust (Map.lookup (x, y) (tiles finalGrid)) == Box) (cartesianProduct [0..w - 1] [0..h - 1])

    print gpsSum

wideInsert :: ((Int, Int), Tile) -> Map (Int, Int) Tile -> Map (Int, Int) Tile
wideInsert ((x, y), t) mp
    | t == Box = do
        let mp' = Map.insert (2 * x, y) LeftBox mp
        Map.insert (2 * x + 1, y) RightBox mp'
    | otherwise = do
        let mp' = Map.insert (2 * x, y) t mp
        Map.insert (2 * x + 1, y) t mp'

widen :: Grid -> Grid
widen Grid {tiles = tiles, robotPos = robotPos, width = width, height = height} = do
    let tiles' = foldr wideInsert Map.empty (Map.toList tiles)
    let robotPos' = (2 * (fst robotPos), snd robotPos)
    Grid {tiles = tiles', robotPos = robotPos', width = 2 * width, height = height}

pushLinked :: (Int, Int) -> (Int, Int) -> Bool -> Grid -> Either () Grid
pushLinked (x, y) (dx, dy) shouldLink grid
    | shouldLink = pushLinked other (dx, dy) False grid >>= pushLinked (x, y) (dx, dy) False
    | destTile == Wall = Left ()
    | destTile == Space = do
        let tiles' = Map.insert dest fromTile (tiles grid)
        let tiles'' = Map.insert (x, y) Space tiles'
        let robotPos' = if (x, y) == robotPos grid then dest else robotPos grid
        Right grid {tiles = tiles'', robotPos = robotPos'}
    | otherwise = pushLinked dest (dx, dy) True grid >>= pushLinked (x, y) (dx, dy) False
    where
        fromTile = fromJust (Map.lookup (x, y) (tiles grid))
        other = if fromTile == RightBox then (x-1, y) else (x+1, y)
        dest = (x+dx, y+dy)
        destTile = fromJust $ Map.lookup dest (tiles grid)

doMoveLinked :: Grid -> (Int, Int) -> Grid
doMoveLinked grid (dx, dy) = do
    let res = pushLinked (robotPos grid) (dx, dy) False grid
    case res of
        Right grid' -> grid'
        Left _ -> grid

second = do
    (grid, moves) <- readGridAndMoves
    let wideGrid = widen grid
    let w = width wideGrid
    let h = height wideGrid

    let finalGrid = foldl doMoveLinked wideGrid moves
    -- print $ robotPos finalGrid
    -- mapM_ (\y -> putStrLn (map (\x -> (toEnum . (13 +) . fromEnum . head . show . fromJust . Map.lookup (x, y)) (tiles finalGrid)) [0..w-1])) [0..h-1]
    let gpsSum = sum $ map calcGps $ filter (\(x, y) -> fromJust (Map.lookup (x, y) (tiles finalGrid)) == LeftBox) (cartesianProduct [0..w - 1] [0..h - 1])

    print gpsSum