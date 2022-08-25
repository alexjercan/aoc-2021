module Day03 where

import Data.Char ( digitToInt )
import Control.Arrow ((&&&))
import Util.List (mostCommon, leastCommon)
import Data.List (transpose)

binaryStringToInt :: String -> Int
binaryStringToInt = foldl go 0
    where go acc x = acc * 2 + digitToInt x

listProp :: ([Char] -> Char) -> [[Char]] -> [Char]
listProp _ [xs] = xs
listProp propF xs = c : listProp propF (map tail $ filter ((==c) . head) xs)
    where c = propF $ map head xs

solve1 :: [[Char]] -> Int
solve1 xs = gamma * epsilon
    where gamma = binaryStringToInt $ map mostCommon $ transpose xs
          epsilon = binaryStringToInt $ map leastCommon $ transpose xs

solve2 :: [[Char]] -> Int
solve2 xs = oxygen * co2
    where oxygen = binaryStringToInt $ listProp mostCommon xs
          co2 = binaryStringToInt $ listProp leastCommon xs

solve :: String -> String
solve = show . (solve1 &&& solve2) . lines

