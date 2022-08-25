{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Day02Test where

import Day02
import Test.Framework

sampleInput :: [Command]
sampleInput = [Forward 5, Down 5, Forward 8, Up 3, Down 8, Forward 2]

test_solve1 :: IO ()
test_solve1 = do
    assertEqual 150 $ solve1 sampleInput

test_solve2 :: IO ()
test_solve2 = do
    assertEqual 900 $ solve2 sampleInput
