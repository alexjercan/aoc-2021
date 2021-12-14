module Day14 where

import qualified Data.Map as M
import Util (Parser, upperP, spaceP, stringP, upperSP, parseList, parse)
import GHC.Base (Alternative(many))
import Data.List.Split(splitOn)
import Control.Arrow ((&&&))
import Data.List (sort, group, maximumBy, minimumBy, transpose)
import Data.Function (on)

type Input = (String, M.Map (Char, Char) Char)

reactionP :: Parser ((Char, Char), Char)
reactionP = do
    a <- upperP
    b <- upperP
    spaceP
    stringP "->"
    spaceP
    c <- upperP
    spaceP
    return ((a, b), c)

contentP :: Parser (String, M.Map (Char, Char) Char)
contentP = do
    a <- upperSP
    many spaceP
    b <- many reactionP
    return (a, M.fromList b)

parseContent :: String -> Input
parseContent = either (error . show) id . parse contentP

interleave :: [a] -> [a] -> [a]
interleave xs ys = concat (transpose [xs, ys])

react :: Ord b => [b] -> M.Map (b, b) b -> [b]
react xs m = interleave xs r
    where r = zipWith (curry (m M.!)) xs (tail xs)

solve1 :: Input -> Int
solve1 (xs, m) = maximum lens - minimum lens
    where f ys = react ys m
          lens = map length $ group $ sort $ iterate f xs !! 10

solve2 :: Input -> Int
solve2 (xs, m) = maximum lens - minimum lens
    where f ys = react ys m
          lens = map length $ group $ sort $ iterate f xs !! 40

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
