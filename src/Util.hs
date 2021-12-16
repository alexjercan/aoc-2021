module Util where

import Data.Char (isSpace)
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

hexP :: Parser Char
hexP = oneOf "0123456789ABCDEF"

hexToBinP :: Parser String
hexToBinP = concatMap hexToBin <$> many1 hexP

hexToBin :: Char -> String
hexToBin '0' = "0000"
hexToBin '1' = "0001"
hexToBin '2' = "0010"
hexToBin '3' = "0011"
hexToBin '4' = "0100"
hexToBin '5' = "0101"
hexToBin '6' = "0110"
hexToBin '7' = "0111"
hexToBin '8' = "1000"
hexToBin '9' = "1001"
hexToBin 'A' = "1010"
hexToBin 'B' = "1011"
hexToBin 'C' = "1100"
hexToBin 'D' = "1101"
hexToBin 'E' = "1110"
hexToBin 'F' = "1111"
hexToBin _ = ""

