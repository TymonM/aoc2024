module Day06 where

import System.IO
import Data.Set (Set)
import qualified Data.Set as Set

first :: IO ()
second :: IO ()

main :: IO ()
main = do
    first
    second

data Coord = Coord {
    x :: Int,
    y :: Int
} deriving (Eq, Ord, Show)

data Grid = Grid {
    width :: Int,
    height :: Int,
    guardPos :: Coord,
    guardDirection :: Coord,
    wallPos :: Set Coord,
    vis :: Set Coord,
    directionalvis :: Set (Coord, Coord),
    isDead :: Bool
} deriving (Show)

indexToCoord :: Int -> Int -> Coord
indexToCoord w i = Coord {x = mod i w, y = div i w}

readGrid :: IO Grid
readGrid = do
    raw <- readFile "input/day06.txt"
    let rawLines = lines raw
    let width = length $ head rawLines
    let height = length rawLines
    let oneline = concat rawLines
    let walls = map (indexToCoord width . snd) $ filter (\x -> fst x =='#') (zip oneline [0..])
    let guard = indexToCoord width $ snd $ head $ filter(\x -> fst x =='^') (zip oneline [0..])

    return Grid {
        width = width,
        height = height,
        guardPos = guard,
        guardDirection = Coord {x = 0,y = -1},
        wallPos = Set.fromList walls,
        vis = Set.empty,
        directionalvis = Set.empty,
        isDead = False
    }

isGuardGone :: Grid -> Bool
isGuardGone grid
    | x (guardPos grid) < 0 = True
    | x (guardPos grid) >= width grid = True
    | y (guardPos grid) < 0 = True
    | y (guardPos grid) >= height grid = True
    | otherwise = False

step :: Coord -> Coord -> Coord
step p d = Coord {x = x p + x d, y = y p + y d}

rotate90 :: Coord -> Coord
rotate90 c = Coord {x = -(y c), y = x c}

simulate :: Grid -> Grid
simulate grid
    -- finished:
    | isGuardGone grid = grid
    -- loop:
    | Set.member (curpos, curdirection) (directionalvis grid) = grid {isDead = True}
    -- hit wall:
    | Set.member newpos (wallPos grid) = simulate $ grid {guardDirection = rotate90 curdirection}
    | otherwise = simulate $ grid {guardPos = newpos, vis = Set.insert curpos (vis grid), directionalvis = Set.insert (curpos, curdirection) (directionalvis grid)}
    where
        curpos = guardPos grid
        curdirection = guardDirection grid
        newpos = step curpos curdirection
        

first = do
    grid <- readGrid

    print (Set.size $ vis $ simulate grid)

doesDie :: Grid -> Bool
doesDie grid = isDead $ simulate grid

placeObstruction :: Grid -> Coord -> Grid
placeObstruction grid at
    | at == guardPos grid = grid
    | otherwise = grid {wallPos = Set.insert at (wallPos grid)}

-- sheesh this takes like 6 seconds QWQ so long
second = do
    grid <- readGrid

    let regular = simulate grid

    let workingObstructions = filter doesDie $ map (placeObstruction grid) (Set.elems $ vis regular)

    print (length workingObstructions)
