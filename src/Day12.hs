module Day12 where

import Data.Char (isLower, isUpper)
import Data.List (group, sort)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Control.Arrow ((&&&))

parseContent :: String -> [(String, String)]
parseContent = map parseLine . lines

parseLine :: String -> (String, String)
parseLine line = (head names, names !! 1)
  where
    names = splitOn "-" line

mkGraph :: [(String, String)] -> M.Map String [String]
mkGraph =
    foldl
        (\m (a, b) -> M.insertWith (++) b [a] $ M.insertWith (++) a [b] m)
        M.empty

fixGraph :: M.Map String [String] -> M.Map String [String]
fixGraph = M.map (filter (/= "start")) . M.filterWithKey (\k _ -> k /= "end")

isLowerS :: String -> Bool
isLowerS = all isLower

isUpperS :: String -> Bool
isUpperS = all isUpper

pred1 :: [String] -> String -> Bool
pred1 vs x = isUpperS x || x `notElem` vs

pred2 :: [String] -> String -> Bool
pred2 vs x = pred1 vs x || allUnique vs
    where allUnique = all ((== 1) . length) . group . sort . filter isLowerS

dfs :: ([String] -> String -> Bool) -> String -> [String] -> M.Map String [String] -> [[String]]
dfs f x vs m
    | x == "end" = [vs]
    | otherwise = concatMap (\x' -> dfs f x' (x' : vs) m) (filter (f vs) (m M.! x))

solution1 :: [(String, String)] -> Int
solution1 = length . dfs pred1 "start" [] . fixGraph . mkGraph

solution2 :: [(String, String)] -> Int
solution2 = length . dfs pred2 "start" [] . fixGraph . mkGraph

solve1 :: String -> Int
solve1 = solution1 . parseContent

solve2 :: String -> Int
solve2 = solution2 . parseContent

solve :: String -> String
solve = show . (solve1 &&& solve2)

