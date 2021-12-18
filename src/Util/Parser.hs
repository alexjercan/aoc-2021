module Util.Parser where

import Text.Parsec (Parsec, ParseError, many1, digit, upper, char, string, space, oneOf, optionMaybe)
import qualified Text.Parsec as Parsec
import Data.Char (digitToInt)
import Util.Binary (binToInt)
import Control.Monad (replicateM)

type Parser = Parsec String ()

parse :: Parser a -> String -> Either ParseError a
parse p = Parsec.parse p ""

parseList :: Parser a -> [String] -> [a]
parseList p = either (error . show) id . mapM (parse p)

naturalP :: Parser Int
naturalP = read <$> many1 digit

integerP :: Parser Int
integerP = do
    s <- optionMaybe (oneOf "+-")
    case s of
      Just '-' -> ((-1)*) <$> naturalP
      _        -> naturalP

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

bitP' :: Parser Char
bitP' = oneOf "01"

bitP :: Parser Int
bitP = digitToInt <$> bitP'

bit2P :: Parser Int
bit2P = binToInt <$> replicateM 2 bitP'

bit3P :: Parser Int
bit3P = binToInt <$> replicateM 3 bitP'

bit4P :: Parser Int
bit4P = binToInt <$> replicateM 4 bitP'




