{-# LANGUAGE TupleSections #-}

module Day11 where

import Control.Monad.State
import Data.Char

parseContent :: String -> [[Int]]
parseContent = map parseLine . lines

parseLine :: String -> [Int]
parseLine = map digitToInt

mkInput :: [[Int]] -> [[(Int, Bool)]]
mkInput = map (map (, False))

borderInf :: [[(Int, Bool)]] -> [[(Int, Bool)]]
borderInf ms =
    [replicate n (0, True)] ++
    map (\xs -> (0, True) : xs ++ [(0, True)]) ms ++ [replicate n (0, True)]
  where
    n = 2 + length (head ms)

at :: [[a]] -> Int -> Int -> a
at ms i j = ms !! i !! j

inc :: (Int, Int) -> [[(Int, Bool)]] -> [[(Int, Bool)]]
inc (i, j) vs
    | snd (at vs i j) = vs
    | otherwise = up ++ [xs ++ [(x + 1, v)] ++ ys] ++ down
      where
        (up, l:down) = splitAt i vs
        (xs, (x, v):ys) = splitAt j l

mark :: (Int, Int) -> [[(Int, Bool)]] -> [[(Int, Bool)]]
mark (i, j) ms = up ++ [xs ++ [(x + 1, True)] ++ ys] ++ down
  where
    (up, l:down) = splitAt i ms
    (xs, (x, _):ys) = splitAt j l

reset :: (Int, Int) -> [[(Int, Bool)]] -> [[(Int, Bool)]]
reset (i, j) vs =
    up ++
    [ xs ++
      [ ( if x > 9
              then 0
              else x
        , False)
      ] ++
      ys
    ] ++
    down
  where
    (up, l:down) = splitAt i vs
    (xs, (x, _):ys) = splitAt j l

neighs :: (Int, Int) -> [(Int, Int)]
neighs (i, j) =
    filter
        (/= (i, j))
        [(i', j') | i' <- [i - 1 .. i + 1], j' <- [j - 1 .. j + 1]]

incNeigh :: (Int, Int) -> [[(Int, Bool)]] -> [[(Int, Bool)]]
incNeigh (i, j) ms = foldr inc ms $ neighs (i, j)

type Eval = State [[(Int, Bool)]]

charge :: Eval ()
charge = do
    ms <- get
    let n = length ms
    let m = length $ head ms
    mapM_ (modify . inc) [(i, j) | i <- [1 .. n - 2], j <- [1 .. m - 2]]

flashBfs :: (Int, Int) -> Eval Int
flashBfs (i, j) = do
    ms <- get
    let (x, v) = at ms i j
    if v
        then return 0
        else do
            if x > 9
                then do
                    modify (mark (i, j))
                    modify (incNeigh (i, j))
                    (1 +) <$>
                        foldM
                            (\acc p -> (acc +) <$> flashBfs p)
                            0
                            (neighs (i, j))
                else return 0

flash :: Eval Int
flash = do
    ms <- get
    let n = length ms
    let m = length $ head ms
    foldM
        (\acc p -> (acc +) <$> flashBfs p)
        0
        [(i, j) | i <- [1 .. n - 2], j <- [1 .. m - 2]]

calm :: Eval ()
calm = do
    ms <- get
    let n = length ms
    let m = length $ head ms
    mapM_ (modify . reset) [(i, j) | i <- [1 .. n - 2], j <- [1 .. m - 2]]

step :: Eval Int
step = do
    charge
    f <- flash
    calm
    return f

flashN :: Int -> Eval Int
flashN n = foldM (\acc _ -> (acc +) <$> step) 0 [1 .. n]

checkN :: Int -> [[Int]] -> Bool
checkN n = all (all ((==0) . fst)) . execState (flashN n) .borderInf . mkInput

solution1 :: [[Int]] -> Int
solution1 = evalState (flashN 100) . borderInf . mkInput

solution2 :: [[Int]] -> Int
solution2 = go 0
    where go n ms
            | checkN n ms = n
            | otherwise = go (n+1) ms

solve1 :: String -> Int
solve1 = solution1 . parseContent

solve2 :: String -> Int
solve2 = solution2 . parseContent

solve :: String -> IO ()
solve content = do
    print $ solve1 content
    print $ solve2 content
