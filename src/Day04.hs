{-# LANGUAGE TupleSections #-}
module Day04 where

import Data.Char
import Control.Monad.State
import Data.List (elemIndex, transpose)
import Control.Applicative
import Data.Maybe (fromMaybe, fromJust)

parseContent :: String -> ([Int], [[[Int]]])
parseContent content = (parseReadings $ head xs, parseBingos $ tail xs)
    where xs = lines content
          getNext ys = dropWhile (all isSpace) ys

parseReadings :: String -> [Int]
parseReadings = map read . splitOn ','

parseBingo :: [String] -> [[Int]]
parseBingo = map (map read . words)

parseBingos :: [String] -> [[[Int]]]
parseBingos [] = []
parseBingos xs = parseBingo ys : parseBingos ys'
    where ys = takeWhile (not . all isSpace) (tail xs)
          ys' = dropWhile (not . all isSpace) (tail xs)

type Bingo = State [[[Bool]]]

emptyBingo :: Int -> Int -> [[[Bool]]]
emptyBingo n m = repeat $ replicate n $ replicate m False

markBingo :: (Int, Int) -> Int -> [[[Bool]]] -> [[[Bool]]]
markBingo pos idx bs = xs ++ updateMatIndex pos True b : ys
    where (xs, b:ys) = splitAt idx bs

checkBingo :: Int -> [[[Bool]]] -> Bool
checkBingo idx bs = linesCheck || columnsCheck
    where linesCheck = any ((==True) . all (==True)) (bs !! idx)
          columnsCheck = any ((==True) . all (==True)) $ transpose (bs !! idx)

step :: Int -> Int -> [[Int]] -> Bingo Bool
step r idx xs = do
    let pos = elemMatIndex r xs
    case pos of
        Nothing -> return False
        Just pos -> do
                modify (markBingo pos idx)
                gets (checkBingo idx)

stepAll :: Int -> [[[Int]]] -> Bingo (Maybe Int)
stepAll r bs = do
    cs <- zipWithM (step r) [0..] bs
    if or cs
        then return $ elemIndex True cs
        else return Nothing

stepUntilBingo :: [Int] -> [[[Int]]] -> Bingo (Maybe (Int, Int))
stepUntilBingo [] _ = return Nothing
stepUntilBingo (r:rs) bs = do
    x <- stepAll r bs
    case x of
        Nothing -> stepUntilBingo rs bs
        _ -> return $ (,r) <$> x

solution1 :: [Int] -> [[[Int]]] -> Int
solution1 rs bs = sum fs * r
    where (Just (idx, r), ms) = runState (stepUntilBingo rs bs) (emptyBingo 5 5)
          mask = ms !! idx
          b = bs !! idx
          (ts, fs) = elemsMatWhere mask b

solution2 :: [Int] -> [[[Int]]] -> Int
solution2 rs bs@[bs'] = solution1 rs bs
solution2 rs bs = solution2 rs newbs
    where Just (idx, r) = evalState (stepUntilBingo rs bs) (emptyBingo 5 5)
          (xs, e:ys) = splitAt idx bs
          newbs = xs ++ ys

solve1 :: String -> Int
solve1 content = solution1 rs bs
    where (rs, bs) = parseContent content

solve2 :: String -> Int
solve2 content = solution2 rs bs
    where (rs, bs) = parseContent content

solve :: String -> IO ()
solve filePath = do
    content <- readFile filePath
    print $ solve1 content
    print $ solve2 content

elemsMatWhere :: [[Bool]] -> [[a]] -> ([a], [a])
elemsMatWhere ms xs = (concat $ zipWith elemtsListWhere ms xs,
                        concat $ zipWith elemtsListWhere nms xs)
    where nms = map (map not) ms

elemtsListWhere :: [Bool] -> [a] -> [a]
elemtsListWhere ms xs = map snd $ filter fst $ zip ms xs

elemMatIndex :: Eq a => a -> [[a]] -> Maybe (Int, Int)
elemMatIndex a xs = liftA2 (,) x y
    where y = msum $ map (elemIndex a) xs
          x = msum $ map (elemIndex a) (transpose xs)

updateMatIndex :: (Int, Int) -> a -> [[a]] -> [[a]]
updateMatIndex (i, j) a as = up ++ (left ++ a : right) : down
    where (up, x:down) = splitAt i as
          (left, _:right) = splitAt j x

-- >>> splitOn ',' "abc,def,,ghi"
-- ["abc","def","","ghi"]
-- >>> splitOn ',' ","
-- ["",""]
-- >>> splitOn ',' ",,,"
-- ["","","",""]
--
splitOn :: Char -> String -> [String]
splitOn c = split (== c)

-- >>> split (==',') ",,abc,def,,ghi,"
-- ["","","abc","def","","ghi",""]
--
split :: (Char -> Bool) -> String -> [String]
split _ [] = []
split p t = loop t
  where
    loop s
      | null s' = [l]
      | otherwise = l : loop (unsafeTail s')
      where
        (l, s') = break p s

unsafeTail :: [a] -> [a]
unsafeTail [] = []
unsafeTail xs = tail xs
