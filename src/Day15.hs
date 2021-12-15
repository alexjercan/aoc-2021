module Day15 where

import Control.Arrow ((&&&))
import Control.Monad.State
    ( MonadState(get, put)
    , State
    , evalState
    , execState
    , forM_
    , modify
    )
import Data.Char (digitToInt)
import Data.Function (on)
import Data.List (minimumBy)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Set as S

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
    filter (isValid n m) [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]
  where
    isValid n m (i, j) = 0 <= i && i < n && 0 <= j && j < m

cost :: M.Map (Int, Int) Int -> (Int, Int) -> Int
cost = (M.!)

isTarget :: (Int, Int) -> (Int, Int) -> Bool
isTarget = (==)

answer :: (Int, Int) -> M.Map (Int, Int) Int -> Int
answer = flip (M.!)

type MyState = (S.Set (Int, Int), M.Map (Int, Int) Int)

type Eval = State MyState

minimumVertex :: Ord a => S.Set a -> M.Map a Int -> a
minimumVertex q dist =
    fst . minimumBy (compare `on` snd) $
    filter ((`S.member` q) . fst) $ M.toList dist

handleNeighbor ::
       (Int, Int) -> ((Int, Int) -> Int) -> (Int, Int) -> MyState -> MyState
handleNeighbor u c v (q, dist) = do
    let alt = dist M.! u + c v
    if alt < fromMaybe 9999 (dist M.!? v)
        then (q, M.insert v alt dist)
        else (q, dist)

dijkstraM ::
       ((Int, Int) -> Bool)
    -> ((Int, Int) -> [(Int, Int)])
    -> ((Int, Int) -> Int)
    -> (M.Map (Int, Int) Int -> Int)
    -> Eval Int
dijkstraM isT neighs c a = do
    (q, dist) <- get
    if S.null q
        then return $ a dist
        else do
            let u = minimumVertex q dist
            let q' = S.delete u q
            put (q', dist)
            if isT u
                then return $ a dist
                else do
                    forM_ (neighs u) (modify . handleNeighbor u c)
                    dijkstraM isT neighs c a

dijkstra ::
       (Int, Int)
    -> M.Map (Int, Int) a
    -> ((Int, Int) -> Bool)
    -> ((Int, Int) -> [(Int, Int)])
    -> ((Int, Int) -> Int)
    -> (M.Map (Int, Int) Int -> Int)
    -> Int
dijkstra source vs isT neighs c a =
    evalState
        (dijkstraM isT neighs c a)
        (S.fromList $ M.keys vs, M.singleton source 0)

solution :: [[Int]] -> Int
solution ms =
    dijkstra
        (0, 0)
        vs
        (isTarget (n - 1, m - 1))
        (getNeighbors n m)
        (cost vs)
        (answer (n - 1, m - 1))
  where
    n = length ms
    m = length $ head ms
    indices = [(i, j) | i <- [0 .. n - 1], j <- [0 .. m - 1]]
    vs = M.fromList $ zip indices (map (\(i, j) -> ms !! i !! j) indices)

solve1 :: [[Int]] -> Int
solve1 = solution

solve2 :: [[Int]] -> Int
solve2 = solution . replicateMat 5

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
