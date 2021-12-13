module Main where

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

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitWith, ExitCode (ExitFailure))

solveDay :: Int -> String -> String
solveDay 1 = Day01.solve
solveDay 2 = Day02.solve
solveDay 3 = Day03.solve
solveDay 4 = Day04.solve
solveDay 5 = Day05.solve
solveDay 6 = Day06.solve
solveDay 7 = Day07.solve
solveDay 8 = Day08.solve
solveDay 9 = Day09.solve
solveDay 10 = Day10.solve
solveDay 11 = Day11.solve
solveDay 12 = Day12.solve
solveDay 13 = Day13.solve
solveDay 14 = Day14.solve
solveDay 15 = Day15.solve
solveDay 16 = Day16.solve
solveDay 17 = Day17.solve
solveDay 18 = Day18.solve
solveDay 19 = Day19.solve
solveDay 20 = Day20.solve
solveDay 21 = Day21.solve
solveDay 22 = Day22.solve
solveDay 23 = Day23.solve
solveDay 24 = Day24.solve
solveDay 25 = Day25.solve
solveDay _ = undefined

main :: IO ()
main = getArgs >>= parse >>= interact . solveDay

parse :: [[Char]] -> IO Int
parse ["-h"] = usage >> exitSuccess
parse ["-v"] = version >> exitSuccess
parse []     = read <$> getContents
parse [day]  = return $ read day
parse _      = usage >> exitWith (ExitFailure 1)

usage :: IO ()
usage = putStrLn "Usage: aoc2015 [-vh] day"

version :: IO ()
version = putStrLn "Aoc2015"

