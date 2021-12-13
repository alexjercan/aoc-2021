module Day11 where

import Data.Char (digitToInt)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Control.Arrow ((&&&))

type Board = M.Map (Int, Int) Int

indexMap :: [[Int]] -> Board
indexMap ms = M.fromList $ map (\(i, j) -> ((i, j), ms !! i !! j)) indices
  where
    n = length ms
    m = length $ head ms
    indices = [(i, j) | i <- [0 .. n - 1], j <- [0 .. m - 1]]

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors p@(i, j) =
    filter (/= p) [(i', j') | i' <- [i - 1 .. i + 1], j' <- [j - 1 .. j + 1]]

increment :: Board -> Board
increment = M.map (+ 1)

neighborFlashCount :: Board -> (Int, Int) -> Int
neighborFlashCount m = length . filter (> 9) . mapMaybe (m M.!?) . neighbors

flash :: [(Int, Int)] -> Board -> Board
flash f m =
    if null f'
        then m
        else flash (f' ++ f) m'
  where
    f' = M.keys $ M.filter (> 9) m
    m' =
        M.mapWithKey
            (\k v ->
                 if k `elem` f ++ f'
                     then 0
                     else v + neighborFlashCount m k)
            m

step :: Board -> Board
step = flash [] . increment

parseContent :: String -> [[Int]]
parseContent = map parseLine . lines

parseLine :: String -> [Int]
parseLine = map digitToInt

solution1 :: [[Int]] -> Int
solution1 m =
    sum $
    map (length . M.elems . M.filter (== 0))
        (take 101 $ iterate step $ indexMap m)

solution2 :: [[Int]] -> Int
solution2 m =
    snd $
    head $
    filter
        fst
        (zipWith
             (\b n -> (all (== 0) $ M.elems b, n))
             (iterate step $ indexMap m)
             [0 ..])

solve1 :: String -> Int
solve1 = solution1 . parseContent

solve2 :: String -> Int
solve2 = solution2 . parseContent

solve :: String -> String
solve = show . (solve1 &&& solve2)

