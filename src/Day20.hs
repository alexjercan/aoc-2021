module Day20 where

solve1 :: String -> Int
solve1 content = undefined

solve2 :: String -> Int
solve2 content = undefined

solve :: String -> IO ()
solve filePath = do
    content <- readFile filePath
    print $ solve1 content
    print $ solve2 content
