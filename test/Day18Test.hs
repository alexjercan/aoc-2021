{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Day18Test where

import Day18
import Test.Framework

sampleInput1 :: Input
sampleInput1 =
    [ [ SN 2 0
      , SN 3 5
      , SN 3 8
      , SN 3 1
      , SN 3 7
      , SN 3 9
      , SN 3 6
      , SN 2 4
      , SN 3 1
      , SN 3 2
      , SN 3 1
      , SN 3 4
      , SN 2 2
      ]
    , [SN 2 5, SN 3 2, SN 3 8, SN 1 4, SN 1 5, SN 3 9, SN 3 9, SN 2 0]
    , [SN 0 6, SN 3 6, SN 3 2, SN 3 5, SN 3 6, SN 3 7, SN 3 6, SN 3 4, SN 3 7]
    , [SN 2 6, SN 3 0, SN 3 7, SN 2 0, SN 2 9, SN 1 4, SN 2 9, SN 3 9, SN 3 0]
    , [ SN 2 7
      , SN 3 6
      , SN 3 4
      , SN 2 3
      , SN 3 1
      , SN 3 3
      , SN 3 5
      , SN 3 5
      , SN 2 1
      , SN 1 9
      ]
    , [ SN 1 6
      , SN 3 7
      , SN 3 3
      , SN 3 3
      , SN 3 2
      , SN 3 3
      , SN 3 8
      , SN 3 5
      , SN 3 7
      , SN 1 4
      ]
    , [SN 3 5, SN 3 4, SN 3 7, SN 3 7, SN 1 8, SN 2 8, SN 2 3, SN 1 8]
    , [SN 1 9, SN 1 3, SN 2 9, SN 2 9, SN 2 6, SN 3 4, SN 3 9]
    , [ SN 1 2
      , SN 3 7
      , SN 3 7
      , SN 2 7
      , SN 2 5
      , SN 2 8
      , SN 3 9
      , SN 3 3
      , SN 3 0
      , SN 3 2
      ]
    , [ SN 3 5
      , SN 3 2
      , SN 2 5
      , SN 2 8
      , SN 3 3
      , SN 3 7
      , SN 2 5
      , SN 3 7
      , SN 3 5
      , SN 2 4
      , SN 2 4
      ]
    ]

test_solve1 :: IO ()
test_solve1 = do
    assertEqual 4140 $ solve1 sampleInput1

test_solve2 :: IO ()
test_solve2 = do
    assertEqual 3993 $ solve2 sampleInput1
