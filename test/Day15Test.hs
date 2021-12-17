{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Day15Test where

import Day15
import Test.Framework

sampleInput1 :: Input
sampleInput1 = [[2, 3]]

test_solve1 :: IO ()
test_solve1 = do
    assertEqual 3 $ solve1 sampleInput1

test_solve2 :: IO ()
test_solve2 = do
    assertEqual 58 $ solve2 sampleInput1
