{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Day16Test where

import Day16
import Test.Framework

sampleInput1 :: Input
sampleInput1 = Literal 6 [1]

test_solve1 :: IO ()
test_solve1 = do
    assertEqual 6 $ solve1 sampleInput1

test_solve2 :: IO ()
test_solve2 = do
    assertEqual 1 $ solve2 sampleInput1
