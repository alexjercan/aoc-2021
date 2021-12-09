module Day09 where

import Data.Char

parseContent :: String -> [[Int]]
parseContent = map parseLine . lines

parseLine :: String -> [Int]
parseLine = map digitToInt

borderInf :: [[Int]] -> [[Int]]
borderInf ms = [replicate n 69] ++ map (\xs -> 69 : xs ++ [69]) ms ++ [replicate n 69]
    where n = 2 + length (head ms)

at :: [[Int]] -> Int -> Int -> Int
at ms i j = (ms !! i) !! j

isLow :: [[Int]] -> (Int, Int) -> Bool
isLow ms (i, j) =
    at ms i j < at ms (i-1) j &&
    at ms i j < at ms (i+1) j &&
    at ms i j < at ms i (j-1) &&
    at ms i j < at ms i (j+1)

solution1 :: [[Int]] -> Int
solution1 ms = sum $ map ((+1) . uncurry (at ms')) (filter (isLow ms') [(i, j)  | i <- [1..n], j <- [1..m]])
    where n = length ms
          m = length $ head ms
          ms' = borderInf ms

solution2 :: [[Int]] -> Int
solution2 = undefined

solve1 :: String -> Int
solve1 = solution1 . parseContent

solve2 :: String -> Int
solve2 = solution2 . parseContent

solve :: String -> IO ()
solve content = do
    print $ solve1 content
    print $ solve2 content

