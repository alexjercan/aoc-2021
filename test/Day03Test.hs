{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Day03Test where

import Day03
import Test.Framework

sampleInput :: String
sampleInput = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010\n"

test_solve1 :: IO ()
test_solve1 = do
    assertEqual 198 $ solve1 sampleInput

test_solve2 :: IO ()
test_solve2 = do
    assertEqual 230 $ solve2 sampleInput

