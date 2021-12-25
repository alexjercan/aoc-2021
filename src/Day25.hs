module Day25 where

import Control.Arrow (Arrow((&&&)))
import qualified Data.Set as S

wrap :: Integral a => a -> a -> a -> a
wrap l h x = l + (x `mod` (h - l))

data Point a =
    Point a a
    deriving (Show, Ord, Eq)

type Input = (S.Set (Point Int), S.Set (Point Int), Int, Int)

parseContent :: String -> Input
parseContent inp = (rights, downs, n, m)
  where
    ms = lines inp
    n = length ms
    m = length $ head ms
    indices = [Point i j | i <- [0 .. n - 1], j <- [0 .. m - 1]]
    xs = zip indices (concat ms)
    rights = S.fromList $ map fst $ filter ((== '>') . snd) xs
    downs = S.fromList $ map fst $ filter ((== 'v') . snd) xs

ifThen :: (p -> Bool) -> (p -> p) -> p -> p
ifThen f g x =
    if f x'
        then x'
        else x
  where
    x' = g x

stepOne :: Ord b => (b -> b) -> S.Set b -> S.Set b -> S.Set b
stepOne move xs ys = S.map (ifThen (`notElem` zs) move) xs
  where
    zs = S.union xs ys

step :: Input -> Input
step (xs, ys, n, m) = (xs', ys', n, m)
  where
    xs' = stepOne moveR xs ys
    ys' = stepOne moveD ys xs'
    moveR (Point x y) = Point x (wrap 0 m (y + 1))
    moveD (Point x y) = Point (wrap 0 n (x + 1)) y

converge :: Eq a => (a -> a) -> a -> Int
converge f =
    (+ 1) . length . takeWhile not . (zipWith (==) <*> tail) . iterate f

solve1 :: Input -> Int
solve1 = converge step

solve2 :: Input -> ()
solve2 = const ()

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
