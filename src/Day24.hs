module Day24 where

import Control.Arrow (Arrow((&&&)))
import Data.List.Split (splitOn)

type Input = String

parseContent :: String -> ([Integer], [Integer])
parseContent = unzip . map g . tail . splitOn ["inp w"] . lines
  where
    g gr = (read (words (gr !! 4) !! 2), read (words (gr !! 14) !! 2))

accumulator ::
       ((Integer, Integer), [(Int, Integer)])
    -> (Int, Integer, Integer)
    -> ((Integer, Integer), [(Int, Integer)])
accumulator ((p, q), stack) (i, a, b)
    | a > 0 = ((p, q), (i, b) : stack)
    | otherwise =
        ( ( p -
            abs (a + b') *
            10 ^
            (13 -
             if a > (-b')
                 then j
                 else i)
          , q +
            abs (a + b') *
            10 ^
            (13 -
             if a < (-b')
                 then j
                 else i))
        , stack')
  where
    ((j, b'):stack') = stack

solution :: [Integer] -> [Integer] -> (Integer, Integer)
solution as bs =
    fst $
    foldl accumulator ((99999999999999, 11111111111111), []) $
    zip3 [0 .. 13] as bs

solve1 :: ([Integer], [Integer]) -> Integer
solve1 = fst . uncurry solution

solve2 :: ([Integer], [Integer]) -> Integer
solve2 = snd . uncurry solution

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
