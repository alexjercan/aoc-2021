module Day12 where

import Control.Monad.State
import Data.Char (isLower)
import Data.List (group, sort)
import Data.List.Split (splitOn)
import qualified Data.Map as M

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

isLowerS :: String -> Bool
isLowerS = all isLower

type Eval = State [String]

dfs :: String -> [[String]] -> [String] -> M.Map String [String] -> [[String]]
dfs x res vs m
    | x == "end" = (x : vs) : res
    | isLowerS x =
        concatMap
            (\x' -> dfs x' res (x : vs) m)
            (filter (`notElem` vs) (m M.! x))
    | otherwise =
        concatMap (\x' -> dfs x' res vs m) (filter (`notElem` vs) (m M.! x))

allUnique :: [String] -> Bool
allUnique = all ((== 1) . length) . group . sort . filter isLowerS

myFilter :: [String] -> String -> Bool
myFilter vs x
    | x == "start" = False
    | x `notElem` vs || allUnique vs = True
    | otherwise = False

dfs2 :: String -> [[String]] -> [String] -> M.Map String [String] -> [[String]]
dfs2 x res vs m
    | x == "end" = (x : vs) : res
    | isLowerS x =
        concatMap
            (\x' -> dfs2 x' res (x : vs) m)
            (filter (myFilter (x : vs)) (m M.! x))
    | otherwise =
        concatMap (\x' -> dfs2 x' res vs m) (filter (myFilter vs) (m M.! x))

solution1 :: [(String, String)] -> Int
solution1 = length . dfs "start" [] [] . mkGraph

solution2 :: [(String, String)] -> Int
solution2 = length . dfs2 "start" [] [] . mkGraph

solve1 :: String -> Int
solve1 = solution1 . parseContent

solve2 :: String -> Int
solve2 = solution2 . parseContent

solve :: String -> IO ()
solve content = do
    print $ solve1 content
    print $ solve2 content
