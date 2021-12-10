{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
module Day10 where

import Control.Monad.State
import Data.List (sort)

parseContent :: String -> [String]
parseContent = lines

type Eval = State (String, String)

mappingP :: Char -> Char
mappingP '(' = ')'
mappingP '[' = ']'
mappingP '{' = '}'
mappingP '<' = '>'
mappingP _ = undefined

errorMap :: Char -> Int
errorMap ')' = 3
errorMap ']' = 57
errorMap '}' = 1197
errorMap '>' = 25137
errorMap _ = undefined

costMap :: Char -> Int
costMap '(' = 1
costMap '[' = 2
costMap '{' = 3
costMap '<' = 4
costMap _ = undefined

checkLine :: Eval Int
checkLine = do
    (xs, ss) <- get
    case xs of
      (y:ys) -> if y `elem` "([{<"
                   then put (ys, y:ss) >> checkLine
                   else case ss of
                          (z:zs) -> if mappingP z == y
                                       then put (ys, zs) >> checkLine
                                       else return $ errorMap y
                          _ -> return $ errorMap y
      _ -> return 0

evalLine :: String -> Int
evalLine xs = evalState checkLine (xs, "")

computeCost :: String -> Int
computeCost = foldl (\acc x -> acc * 5 + x) 0 . map costMap

getMiddleScore :: [Int] -> Int
getMiddleScore xs = xs !! m
    where m = length xs `div` 2

execLine :: String -> String
execLine xs = snd $ execState checkLine (xs, "")

solution1 :: [String] -> Int
solution1 = sum . map evalLine

solution2 :: [String] -> Int
solution2 = getMiddleScore . sort . map (computeCost . execLine) . filter ((==0) . evalLine)

solve1 :: String -> Int
solve1 = solution1 . parseContent

solve2 :: String -> Int
solve2 = solution2 . parseContent

solve :: String -> IO ()
solve content = do
    print $ solve1 content
    print $ solve2 content

