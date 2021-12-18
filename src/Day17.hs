module Day17 where

import Control.Arrow ((&&&))
import Util.Parser (stringP, integerP, Parser, parse)
import Data.Maybe (isJust, fromJust)

data TargetRange = TargetRange Int Int Int Int deriving Show

targetRangeP :: Parser TargetRange
targetRangeP = do
    _ <- stringP "target area: x="
    x0 <- integerP
    _ <- stringP ".."
    x1 <- integerP
    _ <- stringP ", y="
    y0 <- integerP
    _ <- stringP ".."
    y1 <- integerP
    return $ TargetRange (min x0 x1) (max x0 x1) (min y0 y1) (max y0 y1)

type Input = TargetRange

parseContent :: String -> Input
parseContent = either (error . show) id . parse targetRangeP

simulate :: TargetRange -> (Int, Int) -> Maybe Int
simulate = go (0, 0) Nothing
    where go (x, y) my t@(TargetRange x0 x1 y0 y1) (dx, dy)
            | x0 <= x && x <= x1 && y0 <= y && y <= y1 = my
            | (x > x1 && y > y1) || (dx == 0 && (x < x0 || x1 < x)) || (dy < 0 && y < y0) = Nothing
            | otherwise = go (x + dx, y + dy) (max (Just $ y + dy) my) t (xacc dx, dy - 1)
          xacc dx
            | dx > 0 = dx - 1
            | dx < 0 = dx + 1
            | otherwise = 0

solution :: TargetRange -> [Maybe Int]
solution t = map (simulate t) vis
    where vis = [(i, j) | i <- [0..1000], j <- [-100..1000]]

solve1 :: TargetRange -> Int
solve1 = fromJust . maximum . solution

solve2 :: TargetRange -> Int
solve2 t = length $ filter isJust (solution t)

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
