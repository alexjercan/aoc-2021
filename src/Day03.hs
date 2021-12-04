module Day03 where

import Data.List ( maximumBy, minimumBy, group, sort, transpose )
import Data.Function ( on )
import Data.Char ( digitToInt )

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

listProp :: ([Char] -> Char) -> [[Char]] -> [Char]
listProp _ [xs] = xs
listProp propF xs = c : listProp propF (map tail $ filter ((==c) . head) xs)
    where c = propF $ map head xs

solution2 :: [[Char]] -> Int
solution2 xs = oxygen * co2
    where oxygen = binaryStringToInt $ listProp mostCommon xs
          co2 = binaryStringToInt $ listProp leastCommon xs

solve1 :: String -> Int
solve1 content = solution1 $ lines content

solve2 :: String -> Int
solve2 content = solution2 $ lines content

solve :: String -> IO ()
solve content = do
    print $ solve1 content
    print $ solve2 content

