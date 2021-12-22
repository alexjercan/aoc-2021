module Day22 where

import Control.Applicative ((<|>))
import Control.Arrow (Arrow((&&&)))
import qualified Data.Array as A
import GHC.Arr (listArray)
import Text.Parsec (anyChar, char, space, string)
import Util.Parser ( charP, integerP, parseList, Parser )
import Data.Maybe ( catMaybes )

data Cuboid =
    Cuboid (Integer, Integer) (Integer, Integer) (Integer, Integer)
    deriving (Show)

data Status
    = On
    | Off
    deriving (Show, Eq)

data Line =
    Line Status Cuboid
    deriving (Show)

cuboid :: Line -> Cuboid
cuboid (Line _ c) = c

status :: Line -> Status
status (Line s _) = s

isOn :: Line -> Bool
isOn = (==On) . status

type Bounds = ((Integer, Integer, Integer), (Integer, Integer, Integer))

statusP :: Parser Status
statusP = char 'o' >> On <$ char 'n' <|> Off <$ string "ff"

rangeP :: Parser (Integer, Integer)
rangeP = do
    a <- anyChar >> char '=' >> integerP
    b <- string ".." >> integerP
    return (toInteger a, toInteger b + 1)

cuboidP :: Parser Cuboid
cuboidP = do
    a <- rangeP
    b <- charP ',' >> rangeP
    c <- charP ',' >> rangeP
    return $ Cuboid a b c

listP :: Parser Line
listP = do
    s <- statusP
    c <- space >> cuboidP
    return $ Line s c

type Input = [Line]

parseContent :: String -> Input
parseContent = parseList listP . lines

bounds :: [Cuboid] -> Bounds
bounds = bounds' ((0, 0, 0), (0, 0, 0))
  where
    bounds' m [] = m
    bounds' ((xm, ym, zm), (xM, yM, zM)) (Cuboid (xs, xe) (ys, ye) (zs, ze):cs) =
        bounds'
            ( (min xm xs, min ym ys, min zm zs)
            , (max xM xe, max yM ye, max zM ze))
            cs

indices :: Bounds -> Cuboid -> [(Integer, Integer, Integer)]
indices ((xm, ym, zm), (xM, yM, zM)) (Cuboid (xs, xe) (ys, ye) (zs, ze)) =
    [ (x, y, z)
    | x <- [max xm xs .. min xM (xe - 1)]
    , y <- [max ym ys .. min yM (ye - 1)]
    , z <- [max zm zs .. min zM (ze - 1)]
    ]

emptyA :: Bounds -> A.Array (Integer, Integer, Integer) Integer
emptyA bs = listArray bs (repeat 0)

step :: A.Array (Integer, Integer, Integer) Integer -> Line -> A.Array (Integer, Integer, Integer) Integer
step a (Line On c) = a A.// zip (indices (A.bounds a) c) (repeat 1)
step a (Line Off c) = a A.// zip (indices (A.bounds a) c) (repeat 0)

solve1 :: [Line] -> Integer
solve1 = sum . foldl step (emptyA ((-50, -50, -50), (50, 50, 50)))

segmentOverlap :: (Integer, Integer) -> (Integer, Integer) -> Bool
segmentOverlap (xs1, xe1) (xs2, xe2) = xs1 <= xe2 && xs2 <= xe1

cuboidOverlap :: Cuboid -> Cuboid -> Bool
cuboidOverlap (Cuboid x1 y1 z1) (Cuboid x2 y2 z2) =
    segmentOverlap x1 x2 && segmentOverlap y1 y2 && segmentOverlap z1 z2

intersectLeft :: Cuboid -> Cuboid -> (Maybe Cuboid, Cuboid)
intersectLeft c1@(Cuboid (xs1, xe1) y1 z1) (Cuboid (xs2, _) _ _)
    | xs1 < xs2 = (Just $ Cuboid (xs1, xs2) y1 z1, Cuboid (xs2, xe1) y1 z1)
    | otherwise = (Nothing, c1)

intersectRight :: Cuboid -> Cuboid -> (Maybe Cuboid, Cuboid)
intersectRight c1@(Cuboid (xs1, xe1) y1 z1) (Cuboid (_, xe2) _ _)
    | xe2 < xe1 = (Just $ Cuboid (xe2, xe1) y1 z1, Cuboid (xs1, xe2) y1 z1)
    | otherwise = (Nothing, c1)

intersectUp :: Cuboid -> Cuboid -> (Maybe Cuboid, Cuboid)
intersectUp c1@(Cuboid x1 (ys1, ye1) z1) (Cuboid _ (ys2, _) _)
    | ys1 < ys2 = (Just $ Cuboid x1 (ys1, ys2) z1, Cuboid x1 (ys2, ye1) z1)
    | otherwise = (Nothing, c1)

intersectDown :: Cuboid -> Cuboid -> (Maybe Cuboid, Cuboid)
intersectDown c1@(Cuboid x1 (ys1, ye1) z1) (Cuboid _ (_, ye2) _)
    | ye2 < ye1 = (Just $ Cuboid x1 (ye2, ye1) z1, Cuboid x1 (ys1, ye2) z1)
    | otherwise = (Nothing, c1)

intersectFront :: Cuboid -> Cuboid -> (Maybe Cuboid, Cuboid)
intersectFront c1@(Cuboid x1 y1 (zs1, ze1)) (Cuboid _ _ (zs2, _))
    | zs1 < zs2 = (Just $ Cuboid x1 y1 (zs1, zs2), Cuboid x1 y1 (zs2, ze1))
    | otherwise = (Nothing, c1)

intersectBack :: Cuboid -> Cuboid -> (Maybe Cuboid, Cuboid)
intersectBack c1@(Cuboid x1 y1 (zs1, ze1)) (Cuboid _ _ (_, ze2))
    | ze2 < ze1 = (Just $ Cuboid x1 y1 (ze2, ze1), Cuboid x1 y1 (zs1, ze2))
    | otherwise = (Nothing, c1)

intersect :: Cuboid -> Cuboid -> [Cuboid]
intersect c1 c2 = do
    if cuboidOverlap c1 c2
       then do
            let (left, r1) = intersectLeft c1 c2
            let (right, r2) = intersectRight r1 c2
            let (up, r3) = intersectUp r2 c2
            let (down, r4) = intersectDown r3 c2
            let (front, r5) = intersectFront r4 c2
            let (back, _) = intersectBack r5 c2
            catMaybes [left, right, up, down, front, back]
        else [c1]

intersect' :: Line -> Line -> [Line]
intersect' (Line s1 c1) (Line _ c2) = Line s1 <$> intersect c1 c2

step' :: Line -> [Line] -> [Line]
step' x xs = filter isOn (x : concatMap (`intersect'` x) xs)

size' :: Line -> Integer
size' (Line _ (Cuboid (xs, xe) (ys, ye) (zs, ze))) = (xe - xs) * (ye - ys) * (ze - zs)

solve2 :: [Line] -> Integer
solve2 = sum . map size' . foldr step' [] . reverse

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
