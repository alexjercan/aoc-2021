module Day20 where
import Control.Arrow (Arrow((&&&)))
import qualified Data.Map as M
import Util.Extra ( border, indexList, indexMap )
import Data.Maybe ( fromMaybe )
import Util.Binary ( biniToInt )

type Filter = M.Map Int Int
type Image = M.Map (Int, Int) Int

fromChar :: Char -> Int
fromChar '.' = 0
fromChar '#' = 1
fromChar _ = undefined

type Input = (Filter, Image)

parseContent :: String -> Input
parseContent = (indexList . map fromChar . head &&& indexMap . map (map fromChar). tail . tail) . lines

windowAt :: Int ->  Image -> (Int, Int) -> Int
windowAt a im (x, y) = biniToInt ps
    where ks = [(i, j) | i <- [x-1..x+1], j <- [y-1..y+1]]
          ps = map (fromMaybe a . (im M.!?)) ks

filterImage :: Int -> Filter -> Image -> Image
filterImage a fs im = M.fromList $ zip ids $ map ((M.!) fs . windowAt a im) ids
    where n = maximum (map fst $ M.keys im)
          n' = minimum (map fst $ M.keys im)
          m = maximum (map snd $ M.keys im)
          m' = minimum (map snd $ M.keys im)
          ids = [(i, j) | i <- [n'-1 .. n+1], j <- [m'-1 .. m+1]]

solution :: Filter -> Image -> Image
solution fs im = filterImage 1 fs (filterImage 0 fs im)

solve1 :: (Filter, Image) -> Int
solve1 (fs, im)= sum $ solution fs im

solve2 :: (Filter, Image) -> Int
solve2 (fs, im) = sum (iterate (solution fs) im !! 25)

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
