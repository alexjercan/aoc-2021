module Day19 where

import Control.Arrow (Arrow((&&&)))
import Util.Parser (Parser, integerP, charP, stringP, naturalP, manyP, spaceP', parseList, parse, eofP)
import Control.Applicative (Alternative((<|>)))
import Data.List.Split (splitOn)
import Util.Extra (subsets, counter)
import Util.Transform
import Data.List (permutations)
import Data.Maybe (isJust, fromJust, isNothing)
import qualified Data.Map as M
import Control.Monad.State

type Point3DI = Point3D Int
data Scanner = Scanner (Maybe Point3DI) [Point3DI] deriving Show

pos :: Scanner -> Maybe Point3DI
pos (Scanner p _) = p

point3DP :: Parser Point3DI
point3DP = do
    a <- integerP
    _ <- charP ','
    b <- integerP
    _ <- charP ','
    Point3D a b <$> integerP

scannerP :: Parser Scanner
scannerP = do
    _ <- stringP "--- scanner "
    _ <- naturalP
    _ <- stringP " ---"
    _ <- spaceP'
    ps <- manyP (point3DP <* (spaceP' <|> eofP))
    return $ Scanner Nothing ps

type Input = [Scanner]

parseContent :: String -> Input
parseContent = parseList scannerP . splitOn "\n\n"

mkScanners :: [Scanner] -> [Scanner]
mkScanners [] = []
mkScanners ((Scanner _ ps):xss) = Scanner (Just $ Point3D 0 0 0) ps : xss

mkTransformation :: Scanner -> [Scanner]
mkTransformation s = map (tScanP s) transformations
    where tScanP (Scanner p ps) t = Scanner p (map t ps)

overlap :: Scanner -> Scanner -> Scanner
overlap (Scanner (Just p) xs) (Scanner Nothing ys) = Scanner (if M.null counts then Nothing else q) ys
    where diffs = zipWith (-) xs ys
          counts = M.filter (>=12) $ counter diffs
          q = Just $ (+p) $ head $ M.keys counts
overlap s1 s2 = s2

overlap' :: [Scanner] -> Scanner -> Scanner
overlap' vs n = if null vns then n else head vns
    where vns = filter (isJust . pos) (concatMap (\v -> map (overlap v) ns) vs)
          ns = mkTransformation n

type Eval = State [Scanner]

solutionM :: Eval ()
solutionM = do
    ss <- get
    if all (isJust . pos) ss
       then return ()
       else do
        let ns = filter (isNothing . pos) ss
        let vs = filter (isJust . pos) ss
        let ns' = map (overlap' vs) ns
        put $ vs ++ ns'
        solutionM

solution = execState solutionM . mkScanners

solve1 = solution

solve2 = length

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
