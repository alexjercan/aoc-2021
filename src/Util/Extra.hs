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

