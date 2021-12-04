module Day02 where

import Control.Monad.State
    ( execState, MonadState(put, get), State )

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

solution :: (Command -> Move ()) -> [String] -> Int
solution moveM xs = answer $ execState (mapM moveM cmds) (0, 0, 0)
    where
        answer (x, y, _) = x * y
        cmds = map parseCommand xs

solve1 :: String -> Int
solve1 content = solution moveM1 $ lines content

solve2 :: String -> Int
solve2 content = solution moveM2 $ lines content

solve :: String -> IO ()
solve content = do
    print $ solve1 content
    print $ solve2 content

