module Day16 where

import System.IO
import AocUtils (cartesianProduct)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQ
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

firstAndSecond :: IO ()

main :: IO ()
main = firstAndSecond

newtype Coord = Coord (Int, Int) deriving (Show, Eq, Ord)

data Node = Node {
    pos :: Coord,
    direction :: Coord
} deriving (Show, Eq, Ord)

data Tile = Space | Wall | Start | End deriving (Show, Eq)

parseTile :: Char -> Tile
parseTile '.' = Space
parseTile '#' = Wall
parseTile 'S' = Start
parseTile 'E' = End
parseTile _ = error "weird char in input"

readGrid :: IO [[Tile]]
readGrid = (map (map parseTile) . lines) <$> readFile "input/day16.txt"

rotate90 :: Coord -> Coord
rotate90 (Coord (x, y)) = Coord (y, -x)

rotate90' :: Coord -> Coord
rotate90' (Coord (y, negx)) = Coord (-negx, y)

step :: Node -> Node
step (Node (Coord (x, y)) (Coord (dx, dy))) = Node {pos = Coord (x+dx, y+dy), direction = Coord (dx, dy)}

validNeighbours :: [[Tile]] -> Int -> Node -> [(Int, Node)]
validNeighbours grid cost node = filter (notWall . snd) candidates
    where
        dir = direction node
        candidates = [(cost + 1, step node), (cost + 1000, node {direction = rotate90 dir}), (cost + 1000, node {direction = rotate90' dir})]
        notWall (Node (Coord (x, y)) _) = grid !! y !! x /= Wall

dijkstra :: [[Tile]] -> MinPQueue Int Node -> Tile -> Map Node Int -> Map Node Int
dijkstra grid q stopTile best
    | PQ.null q = best
    | Map.member node best = dijkstra grid q' stopTile best
    | grid !! nodey !! nodex == stopTile = best'
    | otherwise = dijkstra grid q'' stopTile best'
    where
        ((cost, node), q') = PQ.deleteFindMin q
        Coord (nodex, nodey) = pos node
        q'' = foldr (uncurry PQ.insert) q' (validNeighbours grid cost node)
        best' = Map.insert node cost best

allDirections :: [Coord]
allDirections = [Coord (0, -1), Coord (1, 0), Coord (0, 1), Coord (-1, 0)]

guardedLookup :: Node -> Map Node Int -> Int
guardedLookup node mp = fromMaybe ((maxBound :: Int) `div` 16) $ Map.lookup node mp

opposite :: Coord -> Coord
opposite (Coord (x, y)) = Coord (-x, -y)

firstAndSecond = do
    grid <- readGrid
    let width = length $ head grid
    let height = length grid

    let startTile = Coord . head $ filter (\(x, y) -> grid !! y !! x == Start) (cartesianProduct [0..width-1] [0..height-1])
    let endTile = Coord . head $ filter (\(x, y) -> grid !! y !! x == End) (cartesianProduct [0..width-1] [0..height-1])
    let distances = dijkstra grid (PQ.singleton 0 $ Node startTile (Coord (1, 0))) End Map.empty
    let ans = minimum $ map (\dir -> guardedLookup (Node endTile dir) distances) allDirections

    print ans

    -- pt 2
    let allEndDistances = map (\dir -> dijkstra grid (PQ.singleton 0 $ Node endTile dir) Start Map.empty) allDirections
    let bestEndDistances = foldr (\(p, dir) mp -> Map.insert (Node p dir) (minimum $ map (guardedLookup (Node p dir)) allEndDistances) mp) Map.empty (cartesianProduct (map Coord $ cartesianProduct [0..width-1] [0..height-1]) allDirections)
    let goodSeatSpots = filter (\p -> any (\dir -> (guardedLookup (Node p dir) distances) + (guardedLookup (Node p(opposite dir)) bestEndDistances) == ans) allDirections) (map Coord $ cartesianProduct [0..width-1] [0..height-1])

    print $ length goodSeatSpots