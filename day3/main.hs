module Main where

import Data.List
import Data.Function
import Data.Char

mostCommon :: Ord a => [a] -> a
mostCommon = head . maximumBy (compare `on` length) . group . sort

leastCommon :: Ord a => [a] -> a
leastCommon = head . minimumBy (compare `on` length) . group . sort

binaryStringToInt :: String -> Int
binaryStringToInt = foldl go 0
    where go acc x = acc * 2 + digitToInt x

solution1 :: [[Char]] -> Int
solution1 xs = gamma * epsilon
    where gamma = binaryStringToInt $ map mostCommon $ transpose xs
          epsilon = binaryStringToInt $ map leastCommon $ transpose xs

solve1 :: String -> IO ()
solve1 inputFile = do
    content <- readFile inputFile
    print (solution1 $ lines content)

listProp :: ([Char] -> Char) -> [[Char]] -> [Char]
listProp _ [xs] = xs
listProp propF xs = c : listProp propF (map tail $ filter ((==c) . head) xs)
    where c = propF $ map head xs

solution2 :: [[Char]] -> Int
solution2 xs = oxygen * co2
    where oxygen = binaryStringToInt $ listProp mostCommon xs
          co2 = binaryStringToInt $ listProp leastCommon xs

solve2 :: String -> IO ()
solve2 inputFile = do
    content <- readFile inputFile
    print (solution2 $ lines content)

main :: IO ()
main = do
    solve1 "sample_input.txt"
    solve2 "sample_input.txt"
