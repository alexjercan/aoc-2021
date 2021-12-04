module Day04 where

import Data.Char ( isSpace )
import Data.List ( maximumBy, minimumBy, (\\), inits, transpose )
import Data.Maybe ( listToMaybe, mapMaybe )
import Data.List.Split ( splitOn )
import Data.Function (on)

orMasks :: [[Bool]] -> [[Bool]] -> [[Bool]]
orMasks = zipWith (zipWith (||))

emptyMask :: [[Bool]]
emptyMask = replicate 5 $ replicate 5 False

marks :: [Int] -> [[Int]] -> [[Bool]]
marks ns b = foldl (\acc x -> orMasks acc $ mark x b) emptyMask ns

mark :: Int -> [[Int]] -> [[Bool]]
mark n = map (map (==n))

isBingo :: [Int] -> [[Int]] -> Bool
isBingo rs bs = (||) <$> anyRows <*> anyCols $ marks rs bs
    where anyRows b = any (all (==True)) b
          anyCols b = any (all (==True)) (transpose b)

whichBingo :: [Int] -> [[Int]] -> Maybe [Int]
whichBingo rs b = listToMaybe $ dropWhile (not . flip isBingo b) irs
    where irs = inits rs

solution1 :: [Int] -> [[[Int]]] -> Int
solution1 rs bs = last x * sum (concat b \\ x)
    where xs = mapMaybe (whichBingo rs) bs
          (x, b) = minimumBy (compare `on` length . fst) $ zip xs bs

solution2 :: [Int] -> [[[Int]]] -> Int
solution2 rs bs = last x * sum (concat b \\ x)
    where xs = mapMaybe (whichBingo rs) bs
          (x, b) = maximumBy (compare `on` length . fst) $ zip xs bs

solve1 :: String -> Int
solve1 content = solution1 rs bs
    where (rs, bs) = parseContent content

solve2 :: String -> Int
solve2 content = solution2 rs bs
    where (rs, bs) = parseContent content

parseContent :: String -> ([Int], [[[Int]]])
parseContent content = (parseReadings $ head xs, parseBingos $ tail xs)
    where xs = lines content

parseReadings :: String -> [Int]
parseReadings = map read . splitOn ","

parseBingo :: [String] -> [[Int]]
parseBingo = map (map read . words)

parseBingos :: [String] -> [[[Int]]]
parseBingos [] = []
parseBingos xs = parseBingo ys : parseBingos ys'
    where ys = takeWhile (not . all isSpace) (tail xs)
          ys' = dropWhile (not . all isSpace) (tail xs)

solve :: String -> IO ()
solve content = do
    print $ solve1 content
    print $ solve2 content

