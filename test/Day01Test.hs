{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Day01Test where

import Day01
import Test.Framework

sampleInput :: IO String
sampleInput = readFile "input/day01/sample_input.txt"

test_solve1 :: IO ()
test_solve1 = do
    content <- sampleInput
    assertEqual 7 $ solve1 content
    assertEqual 5 $ solve2 content

