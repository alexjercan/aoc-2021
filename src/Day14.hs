module Day14 where

import Control.Applicative (Alternative(many))
import Control.Arrow (Arrow((&&&)))
import Control.Monad.State (MonadState(get, put), State, execState, replicateM)
import qualified Data.Map as M
import Util.Parser
    ( Parser
    , parse
    , spaceP
    , stringP
    , upperP
    , upperSP
    )
import Util.Extra ( counter, counterW, mkPairs )

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

insert :: ((Char, Char), Int) -> Char -> [((Char, Char), Int)]
insert ((a, b), v) c = [((a, c), v), ((c, b), v)]

countM :: M.Map (Char, Char) Char -> Eval ()
countM m = do
    (s, f) <- get
    let mids = map (m M.!) $ M.keys s
    let s' = counterW $ concat $ zipWith insert (M.toList s) mids
    let f' = counterW (zip mids (M.elems s))
    put (s', M.unionWith (+) f f')

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
