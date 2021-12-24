module Day24 where

import Control.Arrow (Arrow((&&&)))
import Data.List.Split (splitOn)

-- we have 14 groups of instructions each  starting with "inp w"
-- inp w      W X Y Z
-- mul x 0    W 0 Y Z
-- add x z    W Z Y Z
-- mod x 26   W (Z mod 26) Y Z
-- div z a    W (Z mod 26) Y (Z div a)
-- add x b    W (Z mod 26 + b) Y (Z div a)
-- eql x w    W (W == Z mod 26 + b) Y (Z div a)
-- eql x 0    W (0 == (W == Z mod 26 + b)) Y (Z div a)
-- mul y 0    W (........................) 0 (Z div a)
-- add y 25   W (........................) 25 (Z div a)
-- mul y x    W (...) (25 * (...)) (Z div a)
-- add y 1    W (...) (1 + 25 * (...)) (Z div a)
-- mul z y    W (...) (1 + 25 * (...)) ((Z div a) * (1 + 25 * (...)))
-- mul y 0    W (...) 0 ((Z div a) * (1 + 25 * (...)))
-- add y w    W (...) W ((Z div a) * (1 + 25 * (...)))
-- add y c    W (...) (W + c) ((Z div a) * (1 + 25 * (...)))
-- mul y x    W (...) ((W + c) * (...)) ((Z div a) * (1 + 25 * (...)))
-- add z y    W (...) ((W + c) * (...)) ((W + c) * (...) + ((Z div a) * (1 + 25 * (...))))
-- Z' = (Z div a) + (0 == (W == Z mod 26 + b)) * (W + c + Z div a * 25)

parseContent :: String -> ([Integer], [Integer])
parseContent = unzip . map g . tail . splitOn ["inp w"] . lines
  where g gr = (read (words (gr !! 4) !! 2), read (words (gr !! 14) !! 2))

accumulator :: ((Integer, Integer), [(Int, Integer)]) -> (Int, Integer, Integer) -> ((Integer, Integer), [(Int, Integer)])
accumulator ((p, q), stack) (i, a, b)
    | a > 0 = ((p, q), (i, b) : stack)
    | otherwise = ((p - abs (a + b') * 10 ^ (13 - if a > (-b') then j else i) , q + abs (a + b') * 10 ^ (13 - if a < (-b') then j else i)) , stack')
  where ((j, b'):stack') = stack

solution :: [Integer] -> [Integer] -> (Integer, Integer)
solution as bs = fst $ foldl accumulator ((99999999999999, 11111111111111), []) $ zip3 [0 .. 13] as bs

solve1 :: ([Integer], [Integer]) -> Integer
solve1 = fst . uncurry solution

solve2 :: ([Integer], [Integer]) -> Integer
solve2 = snd . uncurry solution

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
