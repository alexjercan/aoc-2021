module Day19 where

import Control.Arrow (Arrow((&&&)))
import Util.Parser
    ( charP,
      eofP,
      integerP,
      manyP,
      naturalP,
      parseList,
      spaceP',
      stringP,
      Parser )
import Control.Applicative ( Alternative((<|>)) )
import Data.List.Split ( splitOn )
import Util.Extra ( counter )
import Util.Transform ( transformations, Point3D(..) )
import Data.List ( maximumBy, nub )
import Data.Maybe ( isJust, isNothing )
import qualified Data.Map as M
import Control.Monad.State
    ( State, execState, MonadState(put, get) )
import Data.Function ( on )

type Point3DI = Point3D Int
data Scanner = Scanner (Maybe Point3DI) [Point3DI] deriving (Show, Ord, Eq)

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
overlap (Scanner (Just p) xs) (Scanner Nothing ys) = Scanner (if isO then q else Nothing) ys
    where diffs = sequenceA $ concatMap (zipWith (-) xs . repeat) ys
          counts = counter <$> diffs
          counts' = M.toList <$> counts
          mx = maximumBy (compare `on` snd) <$> counts'
          isO = all (>=12) $ snd <$> mx
          q = Just $ (+p) $ fst <$> mx
overlap _ s2 = s2

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

solution :: [Scanner] -> [Scanner]
solution = execState solutionM . mkScanners

computeP :: [Scanner] -> [Point3DI]
computeP ((Scanner (Just p) ps):rs) = map (+p) ps ++ computeP rs
computeP _ = []

solve1 :: [Scanner] -> Int
solve1 = length . nub . computeP . solution

computeM :: [Scanner] -> Int
computeM ss = maximum [dist (s1, s2) | s1 <- ss, s2 <- ss, s1 /= s2]
    where dist (Scanner (Just p1) _, Scanner (Just p2) _) = sum $ abs (p1 - p2)
          dist _ = 0

solve2 :: [Scanner] -> Int
solve2 = computeM . solution

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
