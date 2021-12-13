module Day01 where

import Control.Applicative ( ZipList(ZipList, getZipList) )
import Data.List ( tails )
import Control.Arrow ((&&&))

diff :: [Int] -> [Int]
diff xs = zipWith (-) (tail xs) xs

solution1 :: [Int] -> Int
solution1 = length . filter (>0) . diff

transpose' :: [[a]] -> [[a]]
transpose' = getZipList . traverse ZipList

windows :: Int -> [a] -> [[a]]
windows m = transpose' . take m . tails

solution2 :: [Int] -> Int
solution2 = solution1 . map sum . windows 3

solve1 :: String -> Int
solve1 content = solution1 $ map read $ lines content

solve2 :: String -> Int
solve2 content = solution2 $ map read $ lines content

solve :: String -> String
solve = show . (solve1 &&& solve2)

