module Day09 where

import Control.Monad.State
import Data.Char
import Data.List (sortBy)

parseContent :: String -> [[Int]]
parseContent = map parseLine . lines

parseLine :: String -> [Int]
parseLine = map digitToInt

borderInf :: [[Int]] -> [[Int]]
borderInf ms = [replicate n 9] ++ map (\xs -> 9 : xs ++ [9]) ms ++ [replicate n 9]
    where n = 2 + length (head ms)

at :: [[a]] -> Int -> Int -> a
at ms i j = (ms !! i) !! j

mark :: (Int, Int) -> [[Bool]] -> [[Bool]]
mark (i, j) vs = up ++ [xs ++ [True] ++ ys] ++ down
    where (up, l:down) = splitAt i vs
          (xs, _:ys) = splitAt j l

isLow :: [[Int]] -> (Int, Int) -> Bool
isLow ms (i, j) =
    at ms i j < at ms (i-1) j &&
    at ms i j < at ms (i+1) j &&
    at ms i j < at ms i (j-1) &&
    at ms i j < at ms i (j+1)

getLows :: [[Int]] -> [(Int, Int)]
getLows ms = filter (isLow ms) [(i, j)  | i <- [1..n-1], j <- [1..m-1]]
    where n = length ms
          m = length $ head ms

solution1 :: [[Int]] -> Int
solution1 ms = sum $ map ((+1) . uncurry (at ms')) (getLows ms')
    where ms' = borderInf ms

type Eval = State [[Bool]]

basinSize :: [[Int]] -> (Int, Int) -> Eval Int
basinSize ms (i, j) = do
    modify (mark (i,j))
    a <- do
        vs <- get
        if at ms (i-1) j < 9 && not (at vs (i-1) j)
           then basinSize ms (i-1,j)
           else return 0
    b <- do
        vs <- get
        if at ms (i+1) j < 9 && not (at vs (i+1) j)
           then basinSize ms (i+1,j)
           else return 0
    c <- do
        vs <- get
        if at ms i (j-1) < 9 && not (at vs i (j-1))
           then basinSize ms (i,j-1)
           else return 0
    d <- do
        vs <- get
        if at ms i (j+1) < 9 && not (at vs i (j+1))
           then basinSize ms (i,j+1)
           else return 0
    return $ 1 + a + b + c + d

basinSizes :: [[Int]] -> [(Int, Int)] -> [Int]
basinSizes ms ls = evalState (mapM (basinSize ms) ls) (repeat $ repeat False)

solution2 :: [[Int]] -> Int
solution2 ms = product $ take 3 $ sortBy (flip compare) $ basinSizes ms' (getLows ms')
    where ms' = borderInf ms

solve1 :: String -> Int
solve1 = solution1 . parseContent

solve2 :: String -> Int
solve2 = solution2 . parseContent

solve :: String -> IO ()
solve content = do
    print $ solve1 content
    print $ solve2 content

