module Day13 where

import qualified Data.Map as M
import Data.List.Split (splitOn)
import Control.Arrow ((&&&))
import Data.List (nub, transpose)

parseContent :: String -> ([(Int, Int)], [(Char, Int)])
parseContent = (map parseDot . head &&& map parseFold . (!!1)) . splitOn [""] . lines

parseDot :: String -> (Int, Int)
parseDot = (read . head &&& read . (!!1)) . splitOn ","

parseFold :: String -> (Char, Int)
parseFold = (head . head &&& read . (!!1)) . splitOn "=" . (!!2) . words

mark :: (Int, Int) -> [String] -> [String]
mark (i, j) vs = up ++ [xs ++ ['#'] ++ ys] ++ down
    where (up, l:down) = splitAt i vs
          (xs, _:ys) = splitAt j l

pprint :: [(Int, Int)] -> String
pprint xs = unlines $ transpose $ foldl (flip mark) (replicate n $ replicate m  ' ') xs
    where n = (+1) $ maximum $ map fst xs
          m = (+1) $ maximum $ map snd xs

foldDot :: (Char, Int) -> (Int, Int) -> (Int, Int)
foldDot ('x', v) (x, y) = if x > v then (2 * v - x, y) else (x, y)
foldDot ('y', v) (x, y) = if y > v then (x, 2 * v - y) else (x, y)
foldDot _ _ = undefined

solution1 :: [(Int, Int)] -> (Char, Int) -> Int
solution1 b f = length $ nub $ map (foldDot f) b

solution2 :: [(Int, Int)]-> [(Char, Int)] -> [(Int, Int)]
solution2 = foldl (\acc f -> nub $ map (foldDot f) acc)

solve1 :: String -> Int
solve1 content = solution1 b (head fs)
    where (b, fs) = parseContent content

solve2 :: String -> String
solve2 = pprint . uncurry solution2 . parseContent

solve :: String -> String
solve content = show (solve1 content) ++ "\n" ++  solve2 content

