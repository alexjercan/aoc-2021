{-# LANGUAGE TupleSections #-}
module Day15 where
import Data.Char (digitToInt)
import qualified Data.Map as M
import Control.Arrow ((&&&))

type Input = [[Int]]

parseContent :: String -> Input
parseContent = map (map digitToInt) . lines

sampleInput = "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581\n"

wrap :: (Eq p, Num p) => p -> p
wrap x
    | x == 9 = 1
    | otherwise = x + 1

replicateCol :: (Eq t, Eq b, Num t, Num b) => t -> [[b]] -> [[b]]
replicateCol 1 ms = ms
replicateCol n ms = zipWith (++) ms $ replicateCol (n-1) (map (map wrap) ms)

replicateMat :: (Eq b, Num b, Eq t, Num t) => t -> [[b]] -> [[b]]
replicateMat n = go n
    where go 1 ms = replicateCol n ms
          go i ms = replicateCol n ms ++ go (i-1) (map (map wrap) ms)

initStep :: Input -> M.Map (Int, Int) Int
initStep xs = M.fromList (zip ns row ++ zip ms col)
    where ns = map (0,) [0..length xs - 1]
          ms = map (,0) [0..length (head xs) - 1]
          row = scanl1 (+) $ head xs
          col = scanl1 (+) $ map head xs

dpStep :: Input -> M.Map (Int, Int) Int -> (Int, Int) -> M.Map (Int, Int) Int
dpStep ms dp p@(i, j) = M.insert p ((ms !! i !! j) + min (dp M.! (i-1, j)) (dp M.! (i, j-1))) dp
    where n = length ms
          m = length (head ms)

solution :: [[Int]] -> Int
solution xs = (dp' M.! (n-1,m-1)) - head (head xs)
    where n = length xs
          m = length (head xs)
          dp = initStep xs
          dp' =  foldl (dpStep xs) dp [(i, j) | i <- [1..n-1], j <- [1..m-1]]

solve1 :: [[Int]] -> Int
solve1 = solution

solve2 :: [[Int]] -> Int
solve2 = solution . replicateMat 5

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
