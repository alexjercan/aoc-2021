module Util where

import Data.Char (isSpace, digitToInt)
import Text.Parsec (Parsec, many1, digit, letter, char, upper, space, string, oneOf)
import Text.Parsec.Error (ParseError)
import qualified Text.Parsec as Parsec
import qualified Data.Map as M
import Data.List (transpose)

rstrip :: [Char] -> [Char]
rstrip = reverse . dropWhile isSpace . reverse

interleave :: [a] -> [a] -> [a]
interleave xs ys = concat (transpose [xs, ys])

counter :: Ord a => [a] -> M.Map a Int
counter = foldl (\m x -> M.insertWith (+) x 1 m) M.empty

counterW :: Ord a => [(a, Int)] -> M.Map a Int
counterW = foldl (\m (x, v) -> M.insertWith (+) x v m) M.empty

mkPairs :: [a] -> [(a, a)]
mkPairs = zip <*> tail

type Parser = Parsec String ()

parse :: Parser a -> String -> Either ParseError a
parse p = Parsec.parse p ""

parseList :: Parser a -> [String] -> [a]
parseList p = either (error . show) id . mapM (parse p)

integerP :: Parser Int
integerP = read <$> many1 digit

upperSP :: Parser String
upperSP = many1 upper

charP :: Char -> Parser Char
charP = char

stringP :: String -> Parser String
stringP = string

upperP :: Parser Char
upperP = upper

spaceP :: Parser Char
spaceP = space

manyP :: Parser a -> Parser [a]
manyP = many1

