module Util.Parser where

import Text.Parsec (Parsec, ParseError, many1, digit, upper, char, string, space, oneOf)
import qualified Text.Parsec as Parsec
import Data.Char (digitToInt)
import Util.Binary (binToInt)

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

upto :: Int -> Parser a -> Parser [a]
upto 0 _ = return []
upto n p = (:) <$> p <*> upto (n-1) p

bitP' :: Parser Char
bitP' = oneOf "01"

bitP :: Parser Int
bitP = digitToInt <$> bitP'

bit2P :: Parser Int
bit2P = binToInt <$> upto 2 bitP'

bit3P :: Parser Int
bit3P = binToInt <$> upto 3 bitP'

bit4P :: Parser Int
bit4P = binToInt <$> upto 4 bitP'




