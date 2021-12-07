{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Day07Test where

import Day07
import Test.Framework

sampleInput :: String
sampleInput = "16,1,2,0,4,2,7,1,2,14\n"

test_solve1 :: IO ()
test_solve1 = do
    assertEqual 37 $ solve1 sampleInput

test_solve2 :: IO ()
test_solve2 = do
    assertEqual 168 $ solve2 sampleInput
