{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Day12Test where

import Day12
import Test.Framework

sampleInput1 :: String
sampleInput1 = "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end\n"

sampleInput2 :: String
sampleInput2 = "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc\n"

sampleInput3 :: String
sampleInput3 = "fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW\n"

test_solve1 :: IO ()
test_solve1 = do
    assertEqual 10 $ solve1 sampleInput1
    assertEqual 19 $ solve1 sampleInput2
    assertEqual 226 $ solve1 sampleInput3

test_solve2 :: IO ()
test_solve2 = do
    assertEqual 36 $ solve2 sampleInput1
    assertEqual 103 $ solve2 sampleInput2
    assertEqual 3509 $ solve2 sampleInput3
