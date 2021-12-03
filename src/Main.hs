module Main where
import System.Environment (getArgs)
import qualified Day01
import qualified Day02
import qualified Day03

usage :: IO ()
usage =
    putStrLn "usage"

dayToInputPath :: String -> String
dayToInputPath day = "input/" ++ day ++ "/input.txt"

runDay :: String -> String -> IO ()
runDay "day01" = Day01.solve
runDay "day02" = Day02.solve
runDay "day03" = Day03.solve
runDay _ = error "Not Implemented Yet"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [day] -> runDay day (dayToInputPath day)
        _     -> usage

