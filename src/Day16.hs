module Day16 where
import Control.Arrow
import Numeric (readHex)
import Text.Printf (printf)
import Util (Parser, hexToBinP, manyP, parse, rstrip)

type Input = String

parseContent :: String -> Input
parseContent = either (error . show) id . parse hexToBinP

solve1 = id

solve2 :: Input -> ()
solve2 = const ()

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
