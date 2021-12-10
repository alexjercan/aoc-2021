{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Day10Test where

import Day10
import Test.Framework

sampleInput :: String
sampleInput = "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]\n"

test_solve1 :: IO ()
test_solve1 = do
    assertEqual 26397 $ solve1 sampleInput

test_solve2 :: IO ()
test_solve2 = do
    assertEqual 288957 $ solve2 sampleInput
