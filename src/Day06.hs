module Day06 where
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Control.Monad.State

parseContent :: String -> [Int]
parseContent = map read . splitOn ","

type Memo = State (M.Map Int Int)

handleFish :: Int -> Int -> Memo Int
handleFish n x = do
    memo <- get
    if x `M.member` memo
        then return $ memo M.! x
        else if x < n
            then do
                a <- handleFish n (x + 7)
                b <- handleFish n (x + 9)
                modify $ M.insert x (a + b)
                return $ a + b
            else do
                modify $ M.insert x 1
                return 1

solution :: Int -> [Int] -> Int
solution n xs = sum $ evalState (mapM (handleFish n) xs) M.empty

solve1 :: String -> Int
solve1 = solution 80 . parseContent

solve2 :: String -> Int
solve2 = solution 256 . parseContent

solve :: String -> IO ()
solve content = do
    print $ solve1 content
    print $ solve2 content

