{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Day17Test where

import Day17
import Test.Framework

sampleInput1 :: Input
sampleInput1 = TargetRange 0 0 (-100) (-50)

sampleInput2 :: Input
sampleInput2 = TargetRange 20 30 (-10) (-5)

test_solve1 :: IO ()
test_solve1 = do
    assertEqual 4950 $ solve1 sampleInput1
    assertEqual 45 $ solve1 sampleInput2

test_solve2 :: IO ()
test_solve2 =
    assertEqual 112 $ solve2 sampleInput2
