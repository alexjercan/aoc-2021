module Day16 where

import Control.Arrow ( Arrow((&&&)) )
import Util (Parser, parse)
import Data.Char (digitToInt)
import Text.Parsec (oneOf)

upto :: Int -> Parser a -> Parser [a]
upto 0 p = return []
upto n p = (:) <$> p <*> upto (n-1) p

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
hexToBin _   = ""

binToInt :: String -> Int
binToInt = foldl (\acc x -> acc * 2 + digitToInt x) 0

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


data Op = Sum | Product | Minimum | Maximum | GreaterThan | LessThan | EqualTo deriving Show

toOp :: Int -> Op
toOp 0 = Sum
toOp 1 = Product
toOp 2 = Minimum
toOp 3 = Maximum
toOp 5 = GreaterThan
toOp 6 = LessThan
toOp 7 = EqualTo
toOp _ = undefined

data PacketSize = Length Int | Count Int deriving Show
data Packet = Operator Int Op PacketSize [Packet]
            | Literal Int [Int]
            deriving Show

class BitPacket a where
    bitLength :: a -> Int

instance BitPacket Packet where
    bitLength (Literal _ xs) = 6 + 5 * length xs
    bitLength (Operator _ _ s xs)  = 7 + bitLength s + sum (map bitLength xs)

instance BitPacket PacketSize where
    bitLength (Length _) = 15
    bitLength (Count _) = 11

lento :: (BitPacket a) => Int -> Parser a -> Parser [a]
lento = go 0
    where go m n p
            | m >= n = return []
            | otherwise = do
                x <- p
                (:) x <$> go (m + bitLength x) n p

packetSizeP :: Parser PacketSize
packetSizeP = do
    i <- bitP
    case i of
      0 -> Length . binToInt <$> upto 15 bitP'
      _ -> Count . binToInt <$> upto 11 bitP'

literalPacketContentP :: Parser [Int]
literalPacketContentP = do
    n <- bitP
    (:) <$> bit4P <*> case n of
                        0 -> return []
                        _ -> literalPacketContentP

packetP :: Parser Packet
packetP = do
    v <- bit3P
    t <- bit3P
    case t of
      4 -> Literal v <$> literalPacketContentP
      t -> do
            s <- packetSizeP
            ps <- case s of
              (Length s) -> lento s packetP
              (Count s) -> upto s packetP
            return $ Operator v (toOp t) s ps

type Input = Packet

parseContent :: String -> Input
parseContent = (either (error . show) id . parse packetP) . concatMap hexToBin

cumsumV :: Packet -> Int
cumsumV (Operator v _ _ ps) = v + sum (map cumsumV ps)
cumsumV (Literal v _) = v

solve1 :: Packet -> Int
solve1 = cumsumV

eval :: Packet -> Int
eval (Literal _ xs) = foldl1 (\acc x -> acc * 16 + x) xs
eval (Operator _ Sum _ ps) = sum $ map eval ps
eval (Operator _ Product _ ps) = product $ map eval ps
eval (Operator _ Minimum _ ps) = minimum $ map eval ps
eval (Operator _ Maximum _ ps) = maximum $ map eval ps
eval (Operator _ GreaterThan _ [p1, p2]) = if eval p1 > eval p2 then 1 else 0
eval (Operator _ LessThan _ [p1, p2]) = if eval p1 < eval p2 then 1 else 0
eval (Operator _ EqualTo _ [p1, p2]) = if eval p1 == eval p2 then 1 else 0
eval Operator {} = undefined

solve2 = eval

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
