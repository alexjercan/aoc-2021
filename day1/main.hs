module Main where

import Control.Applicative
import Data.Traversable
import Data.List

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

solve1 :: String -> IO ()
solve1 inputFile = do
    content <- readFile inputFile
    print (solution1 $ map read $ lines content)

solve2 :: String -> IO ()
solve2 inputFile = do
    content <- readFile inputFile
    print (solution2 $ map read $ lines content)

main :: IO ()
main = do
    solve1 "input.txt"
    solve2 "input.txt"
