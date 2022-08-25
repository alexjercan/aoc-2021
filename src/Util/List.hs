module Util.List where

import Data.Function ( on )
import Data.List ( maximumBy, minimumBy, group, sort, tails, transpose )

negate :: Num a => [a] -> [a]
negate = map ((-1)*)

diff :: (Num a) => Int -> [a] -> [a]
diff m = zipWith (-) <*> drop m

slidingWindows :: Int -> [a] -> [[a]]
slidingWindows m = filter ((==m) . length) . transpose . take m . tails

mostCommon :: Ord a => [a] -> a
mostCommon = head . maximumBy (compare `on` length) . group . sort

leastCommon :: Ord a => [a] -> a
leastCommon = head . minimumBy (compare `on` length) . group . sort

