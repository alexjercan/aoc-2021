{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Day01Test
import {-@ HTF_TESTS @-} Day02Test
import {-@ HTF_TESTS @-} Day03Test

main :: IO ()
main = htfMain htf_importedTests
