module Main where

diff :: [Int] -> [Int]
diff xs = zipWith (-) (tail xs) xs

solution :: [Int] -> Int
solution = length . filter (>0) . diff

solve :: String -> IO ()
solve inputFile = do
    content <- readFile inputFile
    print (solution $ map read $ lines content)

main :: IO ()
main = solve "input.txt"
