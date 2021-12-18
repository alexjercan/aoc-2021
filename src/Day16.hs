module Day16 where

import Control.Arrow ( Arrow((&&&)) )
import Util.Parser
    ( Parser, parse, bitP', bitP, bit3P, bit4P )
import Util.Binary ( hexToBin, binToInt )
import Control.Monad (replicateM)

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
      0 -> Length . binToInt <$> replicateM 15 bitP'
      _ -> Count . binToInt <$> replicateM 11 bitP'

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
              (Count s) -> replicateM s packetP
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

solve2 :: Packet -> Int
solve2 = eval

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
