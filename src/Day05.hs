module Day05 where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Control.Arrow ((&&&))

type Point = (Int, Int)
type Line = (Point, Point)
type Board = M.Map (Int, Int) Int

parseLine :: String -> Line
parseLine line = ((x1, y1), (x2, y2))
    where [start, end] = splitOn "->" line
          [x1, y1] = map read $ splitOn "," start
          [x2, y2] = map read $ splitOn "," end

parseContent :: String -> [Line]
parseContent = map parseLine . lines

segmentToPoints :: Line -> [Point]
segmentToPoints ((x1, y1), (x2, y2))
    | x1 == x2 = [(x1, y) | y <- [min y1 y2 .. max y1 y2]]
    | y1 == y2 = [(x, y1) | x <- [min x1 x2 .. max x1 x2]]
    | otherwise = []

segmentToPoints' :: Line -> [Point]
segmentToPoints' ((x1, y1), (x2, y2))
    | abs (y2 - y1) == abs (x2 - x1) = zip (myListComp x1 x2) (myListComp y1 y2)
    | otherwise = []
  where
    myListComp x y
        | x < y = [x..y]
        | otherwise = reverse [y..x]

solution :: [Point] -> Int
solution ps = length $ M.filter (>=2) (M.fromListWith (+) (zip ps (repeat (1::Int))))

solution1 :: [Line] -> Int
solution1 ls = solution $ concatMap segmentToPoints ls

solution2 :: [Line] -> Int
solution2 ls = solution $ concatMap segmentToPoints ls ++ concatMap segmentToPoints' ls

solve1 :: String -> Int
solve1 = solution1 . parseContent

solve2 :: String -> Int
solve2 = solution2 . parseContent

solve :: String -> String
solve = show . (solve1 &&& solve2)

