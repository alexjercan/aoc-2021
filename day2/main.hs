module Main where

import Control.Monad.State

parseCommand :: String -> (String, Int)
parseCommand c = (head cs, read $ cs !! 1)
    where cs = words c

type Submarine = (Int, Int, Int)
type Move = State Submarine
type Command = (String, Int)

moveM1 :: Command -> Move ()
moveM1 ("forward", dx) = do
    (x, y, aim) <- get
    put (x + dx, y, aim)
moveM1 ("down", dy) = do
    (x, y, aim) <- get
    put (x, y + dy, aim)
moveM1 ("up", dy) = do
    (x, y, aim) <- get
    put (x, y - dy, aim)
moveM1 _ = undefined

moveM2 :: Command -> Move ()
moveM2 ("forward", dx) = do
    (x, y, aim) <- get
    put (x + dx, y + aim * dx, aim)
moveM2 ("down", dy) = do
    (x, y, aim) <- get
    put (x, y, aim + dy)
moveM2 ("up", dy) = do
    (x, y, aim) <- get
    put (x, y, aim - dy)
moveM2 _ = undefined

solution1 :: [String] -> Int
solution1 xs = answer $ execState (mapM  moveM1 cmds) (0, 0, 0)
    where
        answer (x, y, _) = x * y
        cmds = map parseCommand xs

solve1 :: String -> IO ()
solve1 inputFile = do
    content <- readFile inputFile
    print (solution1 $ lines content)

solution2 :: [String] -> Int
solution2 xs = answer $ execState (mapM  moveM2 cmds) (0, 0, 0)
    where
        answer (x, y, _) = x * y
        cmds = map parseCommand xs

solve2 :: String -> IO ()
solve2 inputFile = do
    content <- readFile inputFile
    print (solution2 $ lines content)

main :: IO ()
main = do
    solve1 "sample_input.txt"
    solve2 "sample_input.txt"
