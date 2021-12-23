module Day15 where

import Control.Arrow ((&&&))
import Data.Char (digitToInt)
import qualified Data.Map as M
import Util.Search (dijkstra)

type Input = [[Int]]

parseContent :: String -> Input
parseContent = map (map digitToInt) . lines

wrap :: (Eq p, Num p) => p -> p
wrap x
    | x == 9 = 1
    | otherwise = x + 1

replicateCol :: (Eq t, Eq b, Num t, Num b) => t -> [[b]] -> [[b]]
replicateCol 1 ms = ms
replicateCol n ms = zipWith (++) ms $ replicateCol (n - 1) (map (map wrap) ms)

replicateMat :: (Eq b, Num b, Eq t, Num t) => t -> [[b]] -> [[b]]
replicateMat n = go n
  where
    go 1 ms = replicateCol n ms
    go i ms = replicateCol n ms ++ go (i - 1) (map (map wrap) ms)

getNeighbors :: Int -> Int -> (Int, Int) -> [(Int, Int)]
getNeighbors n m (i, j) =
    filter isValid [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]
  where
    isValid (x, y) = 0 <= x && x < n && 0 <= y && y < m

cost :: M.Map (Int, Int) Int -> (Int, Int) -> (Int, Int) -> Int
cost m _ p = m M.! p

isTarget :: (Int, Int) -> (Int, Int) -> Bool
isTarget = (==)

answer ::
       (Int, Int) -> M.Map (Int, Int) Int -> M.Map (Int, Int) (Int, Int) -> Int
answer p dist _ = dist M.! p

solution :: [[Int]] -> Int
solution ms =
    dijkstra
        (0, 0)
        (n - 1, m - 1)
        indices
        (getNeighbors n m)
        (cost vs)
        (answer (n - 1, m - 1))
        9999
  where
    n = length ms
    m = length $ head ms
    indices = [(i, j) | i <- [0 .. n - 1], j <- [0 .. m - 1]]
    vs = M.fromList $ zip indices (map (\(i, j) -> ms !! i !! j) indices)

solve1 :: [[Int]] -> Int
solve1 = solution

solve2 :: [[Int]] -> Int
solve2 = solution . replicateMat (5 :: Int)

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
