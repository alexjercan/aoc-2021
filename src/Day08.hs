module Day08 where

import Data.List (sort)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Control.Arrow ((&&&))

parseContent :: String -> [([String], [String])]
parseContent = map parseDisplay . lines

parseDisplay :: String -> ([String], [String])
parseDisplay xs = (words input, words output)
  where
    [input, output] = splitOn "|" xs

digit1478 :: [a] -> Bool
digit1478 xs =
    length xs == 2 || length xs == 4 || length xs == 3 || length xs == 7

segmentToNumber :: M.Map String Int
segmentToNumber =
    M.fromList
        [ ("abcefg", 0)
        , ("cf", 1)
        , ("acdeg", 2)
        , ("acdfg", 3)
        , ("bcdf", 4)
        , ("abdfg", 5)
        , ("abdefg", 6)
        , ("acf", 7)
        , ("abcdefg", 8)
        , ("abcdfg", 9)
        ]

count :: Ord a => [a] -> M.Map a Int
count = foldl (\m x -> M.insertWith (+) x 1 m) M.empty

uniqF :: [String] -> M.Map Char Int
uniqF = count . concat . filter digit1478

othersF :: [String] -> M.Map Char Int
othersF = count . concat . filter (not . digit1478)

freqsF :: [String] -> M.Map Char Int
freqsF xs = M.unionWith (+) uniq others
    where uniq = uniqF xs
          others = othersF xs

findChar :: String -> Int -> M.Map Char Int -> Char
findChar cs f = fst . M.findMin . M.filterWithKey (\k a -> f == a && notElem k cs)

algorithm :: ([String], [String]) -> Int
algorithm (xs, ys) = do
    let uniq = uniqF xs
    let others = othersF xs
    let freqs = freqsF xs
    let f = findChar [] 9 freqs
    let e = findChar [f] 3 others
    let g = findChar [e,f] 1 uniq
    let d = findChar [e,f,g] 7 freqs
    let b = findChar [d,e,f,g] 6 freqs
    let c = findChar [b,d,e,f,g] 4 others
    let a = findChar [b,c,d,e,f,g] 8 freqs
    let known = M.fromList [(a, 'a'), (b, 'b'), (c, 'c'), (d, 'd'), (e, 'e'), (f, 'f'), (g, 'g')]
    let decoded = map (sort . map (known M.!)) ys
    let numbers = map (segmentToNumber M.!) decoded
    read $ concatMap show numbers

solution1 :: [[String]] -> Int
solution1 = length . filter digit1478 . concat

solution2 :: [([String], [String])] -> Int
solution2 = sum . map algorithm

solve1 :: String -> Int
solve1 = solution1 . map snd . parseContent

solve2 :: String -> Int
solve2 = solution2 . parseContent

solve :: String -> String
solve = show . (solve1 &&& solve2)

