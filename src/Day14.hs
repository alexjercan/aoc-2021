module Day14 where

import Control.Applicative (Alternative(many))
import Control.Arrow (Arrow((&&&)))
import Control.Monad.State (MonadState(get, put), State, execState, replicateM)
import qualified Data.Map as M
import Util
    ( Parser
    , counter
    , counterW
    , mkPairs
    , parse
    , spaceP
    , stringP
    , upperP
    , upperSP
    )

type Input = (String, M.Map (Char, Char) Char)

reactionP :: Parser ((Char, Char), Char)
reactionP = do
    a <- upperP
    b <- upperP
    _ <- spaceP
    _ <- stringP "->"
    _ <- spaceP
    c <- upperP
    _ <- spaceP
    return ((a, b), c)

contentP :: Parser (String, M.Map (Char, Char) Char)
contentP = do
    a <- upperSP
    _ <- many spaceP
    b <- many reactionP
    return (a, M.fromList b)

parseContent :: String -> Input
parseContent = either (error . show) id . parse contentP

type Eval = State (M.Map (Char, Char) Int, M.Map Char Int)

countM :: M.Map (Char, Char) Char -> Eval ()
countM m = do
    (s, f) <- get
    let cs = map (m M.!) $ M.keys s
    let xs1 = zip (zip (map fst (M.keys s)) cs) (M.elems s)
    let xs2 = zip (zip cs (map snd (M.keys s))) (M.elems s)
    let xs = xs1 ++ xs2
    let cn = counterW xs
    let cf = counterW (zip cs (M.elems s))
    put (cn, M.unionWith (+) f cf)

solution :: Int -> Input -> Int
solution n (xs, m) = ((-) <$> maximum <*> minimum) freq
  where
    (_, freq) =
        execState (replicateM n (countM m)) (counter $ mkPairs xs, counter xs)

solve1 :: Input -> Int
solve1 = solution 10

solve2 :: Input -> Int
solve2 = solution 40

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
