{-# LANGUAGE TupleSections #-}
module Day04 where

import Data.Char
import Data.List (elemIndex, transpose)
import Control.Applicative
import Data.Maybe (fromMaybe, fromJust)
import Data.List.Split

type Board = [[(Int, Bool)]]

markNumber :: Int -> [Board] -> [Board]
markNumber n = map (mark n)

mark :: Int -> Board -> Board
mark n = map (map (\(m, y) -> (m, y || m == n)))

isComplete :: Board -> Bool
isComplete board = anyRows || anyCols
    where  asBools = map (map snd) board
           anyRows = any (all (==True)) asBools
           anyCols = any (all (==True)) (transpose asBools)

computeResult :: Int -> Board -> Int
computeResult lastDraw board = lastDraw * s
    where s = sum $ concatMap (map fst . filter ((==False) . snd)) board

solution1 :: [Int] -> [Board] -> Maybe Int
solution1 (x:xs) boards =
    let newBoards = markNumber x boards
        completeBoards = filter isComplete newBoards
        results = map (computeResult x) completeBoards
     in case results of
         [] -> solution1 xs newBoards
         r  -> return $ head r
solution1 _ _ = Nothing

solution2 :: [Int] -> [Board] -> [Board] -> Maybe Int
solution2 (x:xs) rem fin =
    let newRem = markNumber x rem
        newRem' = filter (not . isComplete) newRem
        newFin = fin ++ filter isComplete newRem
     in case newRem' of
         [] -> return $ computeResult x (last newFin)
         _  -> solution2 xs newRem' newFin
solution2 _ _ _ = Nothing

solve1 :: String -> Int
solve1 content = fromJust $ solution1 rs boards
    where (rs, bs) = parseContent content
          boards = map (map $ map (,False)) bs

solve2 :: String -> Int
solve2 content = fromJust $ solution2 rs boards []
    where (rs, bs) = parseContent content
          boards = map (map $ map (,False)) bs

solve :: String -> IO ()
solve filePath = do
    content <- readFile filePath
    print $ solve1 content
    print $ solve2 content

parseContent :: String -> ([Int], [[[Int]]])
parseContent content = (parseReadings $ head xs, parseBingos $ tail xs)
    where xs = lines content
          getNext ys = dropWhile (all isSpace) ys

parseReadings :: String -> [Int]
parseReadings = map read . splitOn ","

parseBingo :: [String] -> [[Int]]
parseBingo = map (map read . words)

parseBingos :: [String] -> [[[Int]]]
parseBingos [] = []
parseBingos xs = parseBingo ys : parseBingos ys'
    where ys = takeWhile (not . all isSpace) (tail xs)
          ys' = dropWhile (not . all isSpace) (tail xs)
