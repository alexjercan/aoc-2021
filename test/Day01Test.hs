{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Day01Test where

import Day01
import Test.Framework

sampleInput :: [Int]
sampleInput = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

test_solve1 :: IO ()
test_solve1 = do
    assertEqual 7 $ solve1 sampleInput

test_solve2 :: IO ()
test_solve2 = do
    assertEqual 5 $ solve2 sampleInput
