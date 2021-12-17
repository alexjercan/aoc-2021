module Day17 where

import Control.Arrow ((&&&))
import Util.Parser (stringP, integerP, Parser, parse)

data TargetRange = TargetRange Int Int Int Int deriving Show

targetRangeP :: Parser TargetRange
targetRangeP = do
    _ <- stringP "target area: x="
    x0 <- integerP
    _ <- stringP ".."
    x1 <- integerP
    _ <- stringP ", y="
    y0 <- integerP
    _ <- stringP ".."
    TargetRange x0 x1 y0 <$> integerP

type Input = TargetRange

parseContent :: String -> Input
parseContent = either (error . show) id . parse targetRangeP

solve1 = id

solve2 :: Input -> ()
solve2 = const ()

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
