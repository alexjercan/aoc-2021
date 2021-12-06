{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Day06Test where

import Day06
import Test.Framework

sampleInput :: String
sampleInput = "3,4,3,1,2\n"

test_solve1 :: IO ()
test_solve1 = do
    assertEqual 5934 $ solve1 sampleInput

test_solve2 :: IO ()
test_solve2 = do
    assertEqual 26984457539 $ solve2 sampleInput
