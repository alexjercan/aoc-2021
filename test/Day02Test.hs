{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Day02Test where

import Day02
import Test.Framework

sampleInput :: String
sampleInput = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2\n"

test_solve1 :: IO ()
test_solve1 = do
    assertEqual 150 $ solve1 sampleInput

test_solve2 :: IO ()
test_solve2 = do
    assertEqual 900 $ solve2 sampleInput
