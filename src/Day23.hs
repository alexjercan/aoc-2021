module Day23 where

import Control.Arrow (Arrow((&&&)))
import Data.Char (isAlpha)
import Data.List (transpose, sort, group)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Util.Search (dijkstra)

data Buffer = Buffer (Maybe Char) (Maybe Char) (Maybe Char) (Maybe Char) (Maybe Char) (Maybe Char) (Maybe Char) deriving (Show, Ord, Eq)
newtype Room = Room [Maybe Char] deriving (Show, Ord, Eq)
data House = House Buffer [Room] deriving (Show, Ord, Eq)

emptyBuffer :: Buffer
emptyBuffer = Buffer Nothing Nothing Nothing Nothing Nothing Nothing Nothing

mkCell :: Char -> Maybe Char
mkCell = Just

mkRoom :: String -> Room
mkRoom = Room <$> map mkCell

mkTargetRooms :: [Room] -> [Room]
mkTargetRooms = map (Room . map Just) . group . sort . concatMap (\(Room xs) -> catMaybes xs)

mkHouse :: [String] -> House
mkHouse xs = House emptyBuffer (map mkRoom (transpose xs))

part2Lines :: [String]
part2Lines = map (filter isAlpha) $ lines "#D#C#B#A#\n#D#B#A#C#\n"

type Input = String

parseContent :: String -> [String]
parseContent = filter (not . null) . map (filter isAlpha) . lines

getNeighbors :: House -> [House]
getNeighbors = undefined

costF :: House -> House -> Int
costF = undefined

answerF :: House -> M.Map House Int -> M.Map House House -> Int
answerF p dist _ = dist M.! p

targetF :: House -> House
targetF (House _ rs) = House emptyBuffer (mkTargetRooms rs)

solution house = dijkstra house target getNeighbors costF (answerF target)
    where target = targetF house

solve1 = mkHouse

solve2 = targetF . mkHouse
-- solve2 xs = dijkstra $ mkHouse (head xs : part2Lines ++ tail xs)

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
