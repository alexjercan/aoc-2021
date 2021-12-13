module Day07 where

import Data.List.Split (splitOn)
import Data.List (sort)
import Control.Arrow ((&&&))

parseContent :: String -> [Int]
parseContent = map read . splitOn ","

cost1 :: [Int] -> Int
cost1 = sum

cost2 :: [Int] -> Int
cost2 = (`quot` 2) . sum . map (\x -> x * (x + 1))

diffAbs :: Num b => b -> [b] -> [b]
diffAbs m = map (abs . flip (-)m)

average :: (Fractional t, Integral a) => (t -> a) -> [a] -> a
average roundF xs = roundF $ fromIntegral (sum xs) / fromIntegral n
    where n = length xs

median :: (Fractional t, Integral a) => (t -> a) -> [a] -> a
median roundF xs
    | n `mod` 2 == 1 = xs' !! m
    | otherwise      = average roundF [xs' !! m, xs' !! (m - 1)]
    where xs' = sort xs
          n = length xs
          m = n `div` 2

solution1 :: [Int] -> Int
solution1 xs = min (cost1 $ diffAbs med1 xs) (cost2 $ diffAbs med2 xs)
    where med1= median (floor :: Float -> Int) xs
          med2 = median (ceiling :: Float -> Int) xs

solution2 :: [Int] -> Int
solution2 xs = min (cost2 $ diffAbs avg1 xs) (cost2 $ diffAbs avg2 xs)
    where avg1 = average (floor :: Float -> Int) xs
          avg2 = average (ceiling :: Float -> Int) xs

solve1 :: String -> Int
solve1 = solution1 . parseContent

solve2 :: String -> Int
solve2 = solution2 . parseContent

solve :: String -> String
solve = show . (solve1 &&& solve2)

