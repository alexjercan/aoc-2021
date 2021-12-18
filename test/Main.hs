{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Day01Test
import {-@ HTF_TESTS @-} Day02Test
import {-@ HTF_TESTS @-} Day03Test
import {-@ HTF_TESTS @-} Day04Test
import {-@ HTF_TESTS @-} Day05Test
import {-@ HTF_TESTS @-} Day06Test
import {-@ HTF_TESTS @-} Day07Test
import {-@ HTF_TESTS @-} Day08Test
import {-@ HTF_TESTS @-} Day09Test
import {-@ HTF_TESTS @-} Day10Test
import {-@ HTF_TESTS @-} Day11Test
import {-@ HTF_TESTS @-} Day12Test
import {-@ HTF_TESTS @-} Day13Test
import {-@ HTF_TESTS @-} Day14Test
import {-@ HTF_TESTS @-} Day15Test
import {-@ HTF_TESTS @-} Day16Test
import {-@ HTF_TESTS @-} Day17Test
{-
import {-@ HTF_TESTS @-} Day18Test
import {-@ HTF_TESTS @-} Day19Test
import {-@ HTF_TESTS @-} Day20Test
import {-@ HTF_TESTS @-} Day21Test
import {-@ HTF_TESTS @-} Day22Test
import {-@ HTF_TESTS @-} Day23Test
import {-@ HTF_TESTS @-} Day24Test
import {-@ HTF_TESTS @-} Day25Test
-}

main :: IO ()
main = htfMain htf_importedTests
