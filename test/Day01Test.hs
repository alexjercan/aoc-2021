{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Day01Test where

import Day01
import Test.Framework

sampleInput :: String
sampleInput = "199\n200\n208\n210\n200\n207\n240\n269\n260\n263\n"

test_solve1 :: IO ()
test_solve1 = do
    assertEqual 7 $ solve1 sampleInput

test_solve2 :: IO ()
test_solve2 = do
    assertEqual 5 $ solve2 sampleInput
