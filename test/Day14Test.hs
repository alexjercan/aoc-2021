{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Day14Test where

import qualified Data.Map as M
import Day14
import Test.Framework

sampleInput1 :: Input
sampleInput1 =
    ( "NNCB"
    , M.fromList
          [ (('C', 'H'), 'B')
          , (('H', 'H'), 'N')
          , (('C', 'B'), 'H')
          , (('N', 'H'), 'C')
          , (('H', 'B'), 'C')
          , (('H', 'C'), 'B')
          , (('H', 'N'), 'C')
          , (('N', 'N'), 'C')
          , (('B', 'H'), 'H')
          , (('N', 'C'), 'B')
          , (('N', 'B'), 'B')
          , (('B', 'N'), 'B')
          , (('B', 'B'), 'N')
          , (('B', 'C'), 'B')
          , (('C', 'C'), 'N')
          , (('C', 'N'), 'C')
          ])

test_solve1 :: IO ()
test_solve1 = assertEqual 1588 $ solve1 sampleInput1

test_solve2 :: IO ()
test_solve2 = assertEqual 2188189693529 $ solve2 sampleInput1
