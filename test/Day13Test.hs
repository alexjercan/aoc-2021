{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Day13Test where

import Day13
import Test.Framework

sampleInput :: String
sampleInput = "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5\n"

test_solve1 :: IO ()
test_solve1 = do
    assertEqual 17 $ solve1 sampleInput
