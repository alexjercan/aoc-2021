module Visual.Day11 where

import Control.Monad
import Data.List.Split (chunksOf)
import qualified Data.Map as M
import Day11
import Graphics.Matplotlib

indexMat :: Board -> [[Int]]
indexMat b = chunksOf m $ map (b M.!) indices
  where
    n = (+ 1) $ maximum $ map fst $ M.keys b
    m = (+ 1) $ maximum $ map fst $ M.keys b
    indices = [(i, j) | i <- [0 .. n - 1], j <- [0 .. m - 1]]

flash' :: [(Int, Int)] -> Board -> [Board]
flash' f m =
    if null f'
        then [m]
        else m : flash' (f' ++ f) m'
  where
    f' = M.keys $ M.filter (> 9) m
    m' =
        M.mapWithKey
            (\k v ->
                 if k `elem` f ++ f'
                     then 0
                     else v + neighborFlashCount m k)
            m

step' :: Board -> [Board]
step' = flash' [] . increment

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

iterate' :: (a -> [a]) -> a -> [a]
iterate' f a = as ++ iterate' f (last as)
    where as = f a

visualize :: String -> IO ()
visualize s = do
    let iter = iterate' step' (indexMap $ parseContent s)
    forM_
        (enumerate $ takeWhile (any (/= 0) . M.elems) iter)
        (\(i, b) ->
             file ("output/" ++ show i ++ ".png") $
             mconcat [(matShow :: [[Int]] -> Matplotlib) $ indexMat b, colorbar])
    putStrLn "Done!"
