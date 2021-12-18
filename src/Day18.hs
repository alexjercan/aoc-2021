module Day18 where

import Control.Applicative (Alternative((<|>)))
import Control.Arrow (Arrow((&&&)))
import Control.Monad (replicateM)
import Util.Parser (Parser, charP, naturalP, parseList)

data SnailNumber =
    SN Int Int
    deriving (Show, Ord, Eq)

level :: SnailNumber -> Int
level (SN x _) = x

value :: SnailNumber -> Int
value (SN _ x) = x

literalP :: Int -> Parser [SnailNumber]
literalP lvl = replicateM 1 $ SN lvl <$> naturalP

pairP :: Int -> Parser [SnailNumber]
pairP lvl = do
    _ <- charP '['
    n1 <- snailNumberP' (lvl + 1)
    _ <- charP ','
    n2 <- snailNumberP' (lvl + 1)
    _ <- charP ']'
    return $ n1 ++ n2

snailNumberP' :: Int -> Parser [SnailNumber]
snailNumberP' lvl = pairP lvl <|> literalP lvl

snailNumberP :: Parser [SnailNumber]
snailNumberP = snailNumberP' (-1)

type Input = [[SnailNumber]]

parseContent :: String -> Input
parseContent = parseList snailNumberP . lines

explode' :: Int -> [SnailNumber] -> [SnailNumber]
explode' x xs@[SN la a, SN lb b, SN _ _]
    | lb == x = [SN la (a + b), SN (lb - 1) 0]
    | otherwise = xs
explode' x ((SN la a):(SN lb b):(SN lc c):(SN ld d):rs)
    | lb == x = SN la (a + b) : SN (lb - 1) 0 : SN ld (c + d) : rs
    | otherwise = SN la a : explode' x (SN lb b : SN lc c : SN ld d : rs)
explode' _ xs = xs

explode :: [SnailNumber] -> [SnailNumber]
explode xs = do
    let x = maximum $ map level xs
    if x < 4
        then xs
        else explode $ tail $ explode' x (SN 0 0 : xs)

split :: [SnailNumber] -> [SnailNumber]
split [] = []
split (SN la a:rs)
    | a > 9 =
        SN (la + 1) (floor $ fromIntegral a / (2 :: Float)) :
        SN (la + 1) (ceiling $ fromIntegral a / (2 :: Float)) : rs
    | otherwise = SN la a : split rs

addition :: [SnailNumber] -> [SnailNumber] -> [SnailNumber]
addition xs ys = f xs ++ f ys
  where
    f = map (\(SN la a) -> SN (la + 1) a)

done :: [SnailNumber] -> Bool
done xs = all ((< 4) . level) xs && all ((< 10) . value) xs

algorithm' :: [SnailNumber] -> [SnailNumber] -> [SnailNumber]
algorithm' xs ys =
    head $ dropWhile (not . done) (iterate (split . explode) (addition xs ys))

algorithm :: [[SnailNumber]] -> [SnailNumber]
algorithm = foldl1 algorithm'

magnitude' :: Int -> [SnailNumber] -> [SnailNumber]
magnitude' x ((SN la a):(SN lb b):rs)
    | la == x = SN (la - 1) (3 * a + 2 * b) : rs
    | otherwise = SN la a : magnitude' x (SN lb b : rs)
magnitude' _ xs = xs

magnitude :: [SnailNumber] -> Int
magnitude xs = do
    let x = maximum $ map level xs
    if x == -1
        then value $ head xs
        else magnitude $ magnitude' x xs

solution :: [[SnailNumber]] -> Int
solution = magnitude . algorithm

solve1 :: [[SnailNumber]] -> Int
solve1 = solution

pairs :: Eq a => [a] -> [[a]]
pairs xs = [[x, y] | x <- xs, y <- xs, x /= y]

solve2 :: [[SnailNumber]] -> Int
solve2 = maximum . map solution . pairs

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
