module Day01 where

import Util.Input
import Util.List
import Control.Arrow ((&&&))

solve1 :: [Int] -> Int
solve1 = length . filter (<0) . diff 1

solve2 :: [Int] -> Int
solve2 = solve1 . map sum . slidingWindows 3

solve :: String -> String
solve = show . (solve1 &&& solve2) . numberColumn

