module Day15 where

import Control.Monad.State
    ( forM_, modify, evalState, MonadState(put, get), State )
import Data.Char (digitToInt)
import qualified Data.Map as M
import Control.Arrow ((&&&))
import qualified Data.Set as S
import Data.List (minimumBy)
import Data.Function (on)
import Data.Maybe (fromMaybe, fromJust)

type Input = [[Int]]

parseContent :: String -> Input
parseContent = map (map digitToInt) . lines

sampleInput = "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581\n"

wrap :: (Eq p, Num p) => p -> p
wrap x
    | x == 9 = 1
    | otherwise = x + 1

replicateCol :: (Eq t, Eq b, Num t, Num b) => t -> [[b]] -> [[b]]
replicateCol 1 ms = ms
replicateCol n ms = zipWith (++) ms $ replicateCol (n-1) (map (map wrap) ms)

replicateMat :: (Eq b, Num b, Eq t, Num t) => t -> [[b]] -> [[b]]
replicateMat n = go n
    where go 1 ms = replicateCol n ms
          go i ms = replicateCol n ms ++ go (i-1) (map (map wrap) ms)

type MyState = (S.Set (Int, Int), M.Map (Int, Int) Int, M.Map (Int, Int) Int)
type Eval = State MyState

isValid :: Int -> Int -> (Int, Int) -> Bool
isValid n m (i, j) = 0 <= i && i < n && 0 <= j && j < m

getNeighbors :: Int -> Int -> (Int, Int) -> [(Int, Int)]
getNeighbors n m (i, j) = filter (isValid n m) [(i+1,j), (i-1,j), (i,j+1), (i,j-1)]

handleNeighbor :: (Int, Int) -> ((Int, Int) -> Int) -> ((Int, Int) -> Int) -> (Int, Int) -> MyState -> MyState
handleNeighbor current h c neighbor (openSet, gScore, fScore) = do
    let g = gScore M.! current + c neighbor
    if g < fromMaybe 9999 (gScore M.!? neighbor)
       then do
           let gScore' = M.insert neighbor g gScore
           let fScore' = M.insert neighbor (g + h neighbor) fScore
           let openSet' = S.insert neighbor openSet
           (openSet', gScore', fScore')
        else do
            (openSet, gScore, fScore)

astarM :: (Int, Int) -> ((Int, Int) -> [(Int, Int)]) -> ((Int, Int) -> Int) -> ((Int, Int) -> Int) -> Eval (Maybe Int)
astarM g neighs h c = do
    (openSet, gScore, fScore) <- get
    if S.null openSet
       then return Nothing
       else do
           let (current, f) = minimumBy (compare `on` snd) $ filter ((`S.member` openSet) . fst) $ M.toList fScore
           if current == g
                  then return $ Just f
                  else do
                      let openSet' = S.delete current openSet
                      put (openSet', gScore, fScore)
                      forM_ (neighs current) (modify . handleNeighbor current h c)
                      astarM g neighs h c

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x1, y1) (x2, y2) = abs (y2 - y1) + abs (x2 - x1)

cost :: [[Int]] -> (Int, Int) -> Int
cost m (i, j) = m !! i !! j

astar :: (Int, Int) -> (Int, Int) -> ((Int, Int) -> [(Int, Int)]) -> ((Int, Int) -> Int) -> ((Int, Int) -> Int) -> Maybe Int
astar s g neighs h c = evalState (astarM g neighs h c) (S.singleton s, M.singleton s 0, M.singleton s $ h s)

solution :: [[Int]] -> Maybe Int
solution ms = astar (0, 0) (n-1, m-1) (getNeighbors n m) (manhattan (n-1, m-1)) (cost ms)
    where n = length ms
          m = length $ head ms

solve1 :: [[Int]] -> Int
solve1 = fromJust . solution

solve2 :: [[Int]] -> Int
solve2 = fromJust . solution . replicateMat 5

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
