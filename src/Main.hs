module Main where

import Text.Printf (printf)
import System.Environment ( getArgs, getEnv )
import System.Exit ( exitSuccess, exitWith, ExitCode(ExitFailure) )
import Control.Monad.Catch ( MonadThrow )
import Control.Monad.IO.Class ( MonadIO )
import Network.HTTP.Conduit
    ( parseRequest, Request(requestHeaders), Response(responseBody) )
import Network.HTTP.Simple ( httpBS )
import Network.HTTP.Types ( hCookie )
import Configuration.Dotenv ( load, loadFile, defaultConfig )

import qualified Data.ByteString.Char8 as B8

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
runDay _ = undefined

readDay :: Int -> IO String
readDay day = do
    readFile $ "input/day" ++ printf "%02d" day ++ ".input"

readDayFromURL :: (MonadThrow m, MonadIO m) => Int -> String -> m String
readDayFromURL day session = do
    initReq <- parseRequest $ "https://adventofcode.com/" ++ "2021/day/" ++ show day ++ "/input"
    response <- httpBS initReq {
          requestHeaders = [(hCookie, B8.pack $ "session=" ++ session)]
    }
    return $ B8.unpack $ responseBody response

mainOnline :: Int -> IO ()
mainOnline day = do
    loadFile defaultConfig >>= load True
    session <- getEnv "AOC_SESSION"
    content <- readDayFromURL day session
    runDay day content

mainOffline ::Int ->  IO ()
mainOffline day = do
    content <- readDay day
    runDay day content

main :: IO ()
main = do
    args <- getArgs
    (method, day) <- parse args
    case method of
        0 -> mainOnline day
        1 -> mainOffline day
        _ -> undefined

parse :: [[Char]] -> IO (Int, Int)
parse ["-h"]            = usage   >> exitSuccess
parse ["-v"]            = version >> exitSuccess
parse []                = read <$> getContents
parse [day]             = return (1, read day)
parse ["--on", day]     = return (0, read day)
parse _                 = usage   >> exitWith (ExitFailure 1)

usage :: IO ()
usage   = putStrLn "Usage: aoc2021 [-vh] [--on] day"

version :: IO ()
version = putStrLn "Aoc2021"

