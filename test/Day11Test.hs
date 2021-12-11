{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Day11Test where

import Day11
import Test.Framework

sampleInput :: String
sampleInput = "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526"

test_solve1 :: IO ()
test_solve1 = do
    assertEqual 1656 $ solve1 sampleInput

test_solve2 :: IO ()
test_solve2 = do
    assertEqual 195 $ solve2 sampleInput
