{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
module Day10 where

import Control.Monad.State
import Data.List (sort)
import Control.Arrow ((&&&))

parseContent :: String -> [String]
parseContent = lines

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

type Eval = State String

checkLine :: String -> Eval Int
checkLine [] = return 0
checkLine (x:xs)  = do
    ss <- get
    if x `elem` "([{<"
       then put (x:ss) >> checkLine xs
       else case ss of
              (z:zs) -> if mappingP z == x
                           then put zs >> checkLine xs
                           else return $ errorMap x
              _ -> return $ errorMap x

evalLine :: String -> Int
evalLine xs = evalState (checkLine xs) ""

execLine :: String -> String
execLine xs = execState (checkLine xs) ""

computeCost :: String -> Int
computeCost = foldl (\acc x -> acc * 5 + x) 0 . map costMap

solution1 :: [String] -> Int
solution1 = sum . map evalLine

solution2 :: [String] -> Int
solution2 = getMiddleScore . sort . map (computeCost . execLine) . filter ((==0) . evalLine)
    where getMiddleScore xs = let m = length xs `div` 2 in xs !! m

solve1 :: String -> Int
solve1 = solution1 . parseContent

solve2 :: String -> Int
solve2 = solution2 . parseContent

solve :: String -> String
solve = show . (solve1 &&& solve2)

