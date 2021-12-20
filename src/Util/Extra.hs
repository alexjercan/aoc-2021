module Util.Extra where

import Data.Char (isSpace)
import Data.List (transpose)
import qualified Data.Map as M

rstrip :: [Char] -> [Char]
rstrip = reverse . dropWhile isSpace . reverse

interleave :: [a] -> [a] -> [a]
interleave xs ys = concat (transpose [xs, ys])

counter :: Ord a => [a] -> M.Map a Int
counter = foldl (\m x -> M.insertWith (+) x 1 m) M.empty

counterW :: Ord a => [(a, Int)] -> M.Map a Int
counterW = foldl (\m (x, v) -> M.insertWith (+) x v m) M.empty

mkPairs :: [a] -> [(a, a)]
mkPairs = zip <*> tail

subsets :: (Eq t, Num t) => t -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x:xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs

allEq :: (Eq a) => [a] -> Bool
allEq [] = True
allEq (x:xs) = all (== x) xs

border :: a -> [[a]] -> [[a]]
border v ms = [replicate n v] ++ map (\xs -> v : xs ++ [v]) ms ++ [replicate n v]
    where n = 2 + length (head ms)

indexMap :: [[a]] -> M.Map (Int, Int) a
indexMap ms = M.fromList $ map (\(i, j) -> ((i, j), ms !! i !! j)) indices
  where
    n = length ms
    m = length $ head ms
    indices = [(i, j) | i <- [0 .. n - 1], j <- [0 .. m - 1]]

indexList :: [a] -> M.Map Int a
indexList xs = M.fromList $ map (\i -> (i, xs !! i)) indices
  where
    n = length xs
    indices = [0 .. n - 1]
