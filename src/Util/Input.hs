module Util.Input where

import Data.List.Split(splitOn)
import Data.Bifunctor (Bifunctor(second))

first2 :: [a] -> (a, a)
first2 (x:y:_) = (x, y)
first2 _ = undefined

numberColumn :: (Read a, Num a) => String -> [a]
numberColumn = map read . lines

stringNumberColumn :: (Read a, Num a) => String -> String -> [(String, a)]
stringNumberColumn sep = map (second read . first2 . splitOn sep) . lines

