module Util.Extra where

import qualified Data.Map as M
import Data.Char (isSpace)
import Data.List (transpose)

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
subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs

allEq :: (Eq a) => [a] -> Bool
allEq [] = True
allEq (x:xs) = all (== x) xs

