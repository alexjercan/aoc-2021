{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Day03Test where

import Day03
import Test.Framework

sampleInput :: IO String
sampleInput = readFile "input/day03/sample_input.txt"

test_solve1 :: IO ()
test_solve1 = do
    content <- sampleInput
    assertEqual 198 $ solve1 content

test_solve2 :: IO ()
test_solve2 = do
    content <- sampleInput
    assertEqual 230 $ solve2 content

