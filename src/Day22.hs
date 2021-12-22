module Day22 where

import Control.Applicative ((<|>))
import Control.Arrow (Arrow((&&&)))
import qualified Data.Array as A
import qualified Data.Set as S
import GHC.Arr (listArray)
import Text.Parsec (anyChar, char, space, string)
import Util.Parser

data Range =
    Range Int Int
    deriving (Show)

data Cuboid =
    Cuboid Range Range Range
    deriving (Show)

data Status
    = On
    | Off
    deriving (Show)

data List =
    List Status Cuboid
    deriving (Show)

cuboid :: List -> Cuboid
cuboid (List _ c) = c

type Bounds = ((Int, Int, Int), (Int, Int, Int))

statusP :: Parser Status
statusP = char 'o' >> ((On <$ char 'n') <|> (Off <$ string "ff"))

rangeP :: Parser Range
rangeP = do
    a <- anyChar >> char '=' >> integerP
    b <- string ".." >> integerP
    return $ Range a b

cuboidP :: Parser Cuboid
cuboidP = do
    a <- rangeP
    b <- charP ',' >> rangeP
    c <- charP ',' >> rangeP
    return $ Cuboid a b c

listP :: Parser List
listP = do
    s <- statusP
    c <- space >> cuboidP
    return $ List s c

type Input = [List]

parseContent :: String -> Input
parseContent = parseList listP . lines

bounds' :: Bounds -> [Cuboid] -> Bounds
bounds' m [] = m
bounds' ((xm, ym, zm), (xM, yM, zM)) (Cuboid (Range xs xe) (Range ys ye) (Range zs ze):cs) =
    bounds' ((min xm xs, min ym ys, min zm ys), (max xM xe, max yM ye, max zM ze)) cs

bounds :: [Cuboid] -> Bounds
bounds = bounds' ((0, 0, 0), (0, 0, 0))

indices :: Bounds -> Cuboid -> [(Int, Int, Int)]
indices ((xm, ym, zm), (xM, yM, zM)) (Cuboid (Range xs xe) (Range ys ye) (Range zs ze)) =
    [ (x, y, z)
    | x <- [max xm xs .. min xM xe]
    , y <- [max ym ys .. min yM ye]
    , z <- [max zm zs .. min zM ze]
    ]

step :: A.Array (Int, Int, Int) Int -> List -> A.Array (Int, Int, Int) Int
step a (List On c) = a A.// zip (indices (A.bounds a) c) (repeat 1)
step a (List Off c) = a A.// zip (indices (A.bounds a) c) (repeat 0)

emptyA :: Bounds -> A.Array (Int, Int, Int) Int
emptyA bs = listArray bs (repeat 0)

solve1 = sum . foldl step (emptyA ((-50, -50, -50), (50, 50, 50)))

solve2 ls = bounds $ map cuboid ls

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
