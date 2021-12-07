module Day07 where

import Data.List.Split (splitOn)
import Data.List (sort)

parseContent :: String -> [Int]
parseContent = map read . splitOn ","

cost1 :: [Int] -> Int
cost1 = sum

cost2 :: [Int] -> Int
cost2 = sum . map (\x -> x * (x + 1) `div` 2)

average :: [Int] -> Int
average xs = round $ fromIntegral (sum xs) / (fromIntegral n :: Float)
    where n = length xs

median :: [Int] -> Int
median xs
    | n `mod` 2 == 1 = xs' !! m
    | otherwise      = average [xs' !! m , xs' !! (m - 1)]
    where xs' = sort xs
          n = length xs
          m = n `div` 2

solution1 :: [Int] -> Int
solution1 xs = sum $ map (\x -> abs $ x - med) xs
    where med = median xs

solution2 :: [Int] -> Int
solution2 xs = minimum costs
    where avg = average xs
          med = median xs
          searchInt = [min avg med .. max avg med]
          costs = map (\i -> cost2 $ map (\x -> abs (x - i)) xs) searchInt

solve1 :: String -> Int
solve1 = solution1 . parseContent

solve2 :: String -> Int
solve2 = solution2 . parseContent

solve :: String -> IO ()
solve content = do
    print $ solve1 content
    print $ solve2 content

