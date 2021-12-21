{-# LANGUAGE DeriveFunctor #-}

module Day21 where

import Control.Arrow (Arrow((&&&)))
import Control.Monad.State
    ( State, modify, evalState, MonadState(get) )
import Data.Foldable (foldlM)
import qualified Data.Map as M
import Util.Parser ( naturalP, parse, stringP, Parser )

cycle' :: Int -> Int -> Int
cycle' m x = (x - 1) `mod` m + 1

cycle10 :: Int -> Int
cycle10 = cycle' 10

cycle100 :: Int -> Int
cycle100 = cycle' 100

type Input = (Int, Int)

inputP :: Parser Input
inputP = do
    _ <- stringP "Player 1 starting position: "
    a <- naturalP
    _ <- stringP "\nPlayer 2 starting position: "
    b <- naturalP
    return (a, b)

parseContent :: String -> Input
parseContent = either (error . show) id . parse inputP

data Game =
    Game Int Int Int Int
    deriving (Show, Ord, Eq)

data Dice =
    Dice Int Int
    deriving (Show)

type DetGame = (Game, Dice)

mkDetGame :: Input -> DetGame
mkDetGame (a, b) = (Game a 0 b 0, Dice 1 0)

step :: DetGame -> DetGame
step (Game pa sa pb sb, Dice d n) =
    (Game pb sb pa' sa', Dice (cycle100 (d + 3)) (n + 3))
  where
    pa' = cycle10 (pa + valueDice d `mod` 10)
    sa' = sa + pa'
    valueDice x = cycle100 x + cycle100 (x + 1) + cycle100 (x + 2)

checkEnd :: DetGame -> Bool
checkEnd (Game pa sa pb sb, _) = sa >= 1000 || sb >= 1000

solve1 :: Input -> Int
solve1 = answer . head . dropWhile (not . checkEnd) . iterate step . mkDetGame
  where
    answer (Game pa sa pb sb, Dice _ n) = min sa sb * n

data Splits' a =
    Splits a a
    deriving (Show, Functor)

instance Applicative Splits' where
    pure a = Splits a a
    (<*>) (Splits fa fb) (Splits a b) = Splits (fa a) (fb b)

flipSplits :: Splits' a -> Splits' a
flipSplits (Splits a b) = Splits b a

maximumSplits :: Ord a => Splits' a -> a
maximumSplits (Splits a b) = max a b

type Splits = Splits' Integer

type DirGame = (Game, Splits)

mkDirGame :: Input -> Game
mkDirGame (a, b) = Game a 0 b 0

rolls :: [(Int, Integer)]
rolls = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]

type Eval = State (M.Map Game Splits)

stepBfsM :: Game -> Splits -> (Int, Integer) -> Eval Splits
stepBfsM g@(Game pa sa pb sb) w (roll, freq) = do
    s <- solutionM (Game pb sb pa' sa')
    return $ (+) <$> w <*> ((* freq) <$> flipSplits s)
  where
    pa' = cycle10 (pa + roll)
    sa' = sa + pa'

solutionM :: Game -> Eval Splits
solutionM g@(Game pa sa pb sb)
    | sb >= 21 = return $ Splits 0 1
    | otherwise = do
        ms <- get
        if g `M.member` ms
            then return $ ms M.! g
            else do
                s <- foldlM (stepBfsM g) (Splits 0 0) rolls
                modify (M.insert g s)
                return s

solve2 :: Input -> Integer
solve2 s = maximumSplits $ evalState (solutionM (mkDirGame s)) M.empty

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
