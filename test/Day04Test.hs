{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Day04Test where

import Day04
import Test.Framework

sampleInput :: IO String
sampleInput = readFile "input/day04/sample_input.txt"

test_solve1 :: IO ()
test_solve1 = do
    content <- sampleInput
    assertEqual 4512 $ solve1 content

