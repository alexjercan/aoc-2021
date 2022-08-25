module Day02 where

import Control.Arrow ((&&&))
import Util.Input (stringNumberColumn)

data Command = Forward Int | Down Int | Up Int deriving Show

toCommand :: (String, Int) -> Command
toCommand ("forward", x) = Forward x
toCommand ("down", x) = Down x
toCommand ("up", x) = Up x
toCommand _ = undefined

parseContent :: String -> [Command]
parseContent = map toCommand . stringNumberColumn " "

type Submarine = (Int, Int, Int)

move1 :: Submarine -> Command -> Submarine
move1 (x, y, aim) (Forward dx) = (x + dx, y, aim)
move1 (x, y, aim) (Down dx) = (x, y + dx, aim)
move1 (x, y, aim) (Up dx) = (x, y - dx, aim)

move2 :: Submarine -> Command -> Submarine
move2 (x, y, aim) (Forward dx) = (x + dx, y + aim * dx, aim)
move2 (x, y, aim) (Down dx) = (x, y, aim + dx)
move2 (x, y, aim) (Up dx) = (x, y, aim - dx)

solution :: (Foldable t, Num a1, Num c) => ((a1, a1, c) -> a2 -> (a1, a1, c)) -> t a2 -> a1
solution move = answer . foldl move (0, 0, 0)
    where answer (x, y, _) = x * y

solve1 :: [Command] -> Int
solve1 = solution move1

solve2 :: [Command] -> Int
solve2 = solution move2

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent

