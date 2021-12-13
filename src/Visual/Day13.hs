module Visual.Day13 where

import Control.Monad
import Data.List.Split (chunksOf)
import qualified Data.Map as M
import Day11
import Graphics.Matplotlib
import Control.Arrow ((&&&))

visualize :: [(Int, Int)] -> IO ()
visualize s = do
    onscreen $ uncurry (scatter :: [Int] -> [Int] -> Matplotlib ) $ unzip s
    print "Hi"
