{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Day09Test where

import Day09
import Test.Framework

sampleInput :: String
sampleInput = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678\n"

test_solve1 :: IO ()
test_solve1 = do
    assertEqual 15 $ solve1 sampleInput

test_solve2 :: IO ()
test_solve2 = do
    assertEqual 1134 $ solve2 sampleInput
