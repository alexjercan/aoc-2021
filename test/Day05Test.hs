{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Day05Test where

import Day05
import Test.Framework

sampleInput :: String
sampleInput = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2\n"

test_solve1 :: IO ()
test_solve1 = do
    assertEqual 5 $ solve1 sampleInput

test_solve2 :: IO ()
test_solve2 = do
    assertEqual 12 $ solve2 sampleInput
