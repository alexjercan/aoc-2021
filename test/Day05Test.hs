{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Day05Test where

import Day05
import Test.Framework

sampleInput :: IO String
sampleInput = readFile "input/day05/sample_input.txt"

test_solve1 :: IO ()
test_solve1 = do
    content <- sampleInput
    assertEqual 0 $ solve1 content

test_solve2 :: IO ()
test_solve2 = do
    content <- sampleInput
    assertEqual 0 $ solve2 content
