module Main where

import Text.Printf (printf)
import System.Environment (getArgs)
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25

usage :: IO ()
usage =
    putStrLn "cabal run Aoc2021 {day}"

dayToInputPath :: Int -> String
dayToInputPath day = "input/day" ++ printf "%02d" day ++ "/input.txt"

runDay :: Int -> String -> IO ()
runDay 1 = Day01.solve
runDay 2 = Day02.solve
runDay 3 = Day03.solve
runDay 4 = Day04.solve
runDay 5 = Day05.solve
runDay 6 = Day06.solve
runDay 7 = Day07.solve
runDay 8 = Day08.solve
runDay 9 = Day09.solve
runDay 10 = Day10.solve
runDay 11 = Day11.solve
runDay 12 = Day12.solve
runDay 13 = Day13.solve
runDay 14 = Day14.solve
runDay 15 = Day15.solve
runDay 16 = Day16.solve
runDay 17 = Day17.solve
runDay 18 = Day18.solve
runDay 19 = Day19.solve
runDay 20 = Day20.solve
runDay 21 = Day21.solve
runDay 22 = Day22.solve
runDay 23 = Day23.solve
runDay 24 = Day24.solve
runDay 25 = Day25.solve
runDay _ = error "Day does not exist"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [day] -> runDay (read day) (dayToInputPath $ read day)
        _     -> usage

