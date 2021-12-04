{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Day02Test where

import Day02
import Test.Framework

sampleInput :: IO String
sampleInput = readFile "input/day02/sample_input.txt"

test_solve1 :: IO ()
test_solve1 = do
    content <- sampleInput
    assertEqual 150 $ solve1 content

test_solve2 :: IO ()
test_solve2 = do
    content <- sampleInput
    assertEqual 900 $ solve2 content
