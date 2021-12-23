module Day23 where

import Control.Arrow (Arrow((&&&)))
import Data.Char (isAlpha)
import Data.List (transpose, sort, group, unzip4)
import qualified Data.Map as M
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Util.Search (dijkstra)
import qualified Data.Array as A

init4 :: [a] -> (a, a, a, a)
init4 (a:b:c:d:_) = (a, b, c, d)
init4 _ = undefined

cellFromChar :: Char -> Cell
cellFromChar 'A' = A
cellFromChar 'B' = B
cellFromChar 'C' = C
cellFromChar 'D' = D
cellFromChar _   = N

fromList :: [a] -> A.Array Int a
fromList xs = A.listArray (0, length xs - 1) xs

data Cell = A | B | C | D | N deriving (Show, Ord, Eq)
data Buffer = Buffer Cell Cell Cell Cell Cell Cell Cell deriving (Show, Ord, Eq)
data House = House Buffer (A.Array Int Cell) (A.Array Int Cell) (A.Array Int Cell) (A.Array Int Cell) deriving (Show, Ord, Eq)

emptyBuffer :: Buffer
emptyBuffer = Buffer N N N N N N N

mkConstRoom :: Int -> Cell -> A.Array Int Cell
mkConstRoom n = fromList . replicate n

mkHouse :: Input -> House
mkHouse (r1, r2, r3, r4) = House emptyBuffer (fromList r1) (fromList r2) (fromList r3) (fromList r4)

type Input = ([Cell], [Cell], [Cell], [Cell])

parseContent' :: String -> [(Cell, Cell, Cell, Cell)]
parseContent' = map init4 . filter (not . null) . map (map cellFromChar . filter isAlpha) . lines

parseContent1 :: String -> Input
parseContent1 = unzip4 . parseContent'

parseContent2 :: String -> Input
parseContent2 = unzip4 . f .  parseContent'
    where f xs = head xs : ys ++ tail xs
          ys = parseContent' "#D#C#B#A#\n#D#B#A#C#\n"

cost :: Int -> Cell -> Int
cost i A = i
cost i B = i * 10
cost i C = i * 100
cost i D = i * 1000
cost _ N = 0

emptySpaces :: A.Array Int Cell -> [Int]
emptySpaces arr = takeWhile (\i -> arr A.! i == N) $ A.indices arr

fromBufferToRoom01 (House (Buffer A N c d e f g) r1 r2 r3 r4) = map (\i -> (cost (3+i) A, House (Buffer N N c d e f g) (r1 A.// [(i, A)]) r2 r3 r4)) (emptySpaces r1)
fromBufferToRoom01 _ = []
fromBufferToRoom02 (House (Buffer a A c d e f g) r1 r2 r3 r4) = map (\i -> (cost (2+i) A, House (Buffer a N c d e f g) (r1 A.// [(i, A)]) r2 r3 r4)) (emptySpaces r1)
fromBufferToRoom02 _ = []
fromBufferToRoom03 (House (Buffer a b A d e f g) r1 r2 r3 r4) = map (\i -> (cost (2+i) A, House (Buffer a b N d e f g) (r1 A.// [(i, A)]) r2 r3 r4)) (emptySpaces r1)
fromBufferToRoom03 _ = []
fromBufferToRoom04 (House (Buffer a b N A e f g) r1 r2 r3 r4) = map (\i -> (cost (4+i) A, House (Buffer a b N N e f g) (r1 A.// [(i, A)]) r2 r3 r4)) (emptySpaces r1)
fromBufferToRoom04 _ = []
fromBufferToRoom05 (House (Buffer a b N N A f g) r1 r2 r3 r4) = map (\i -> (cost (6+i) A, House (Buffer a b N N N f g) (r1 A.// [(i, A)]) r2 r3 r4)) (emptySpaces r1)
fromBufferToRoom05 _ = []
fromBufferToRoom06 (House (Buffer a b N N N A g) r1 r2 r3 r4) = map (\i -> (cost (8+i) A, House (Buffer a b N N N N g) (r1 A.// [(i, A)]) r2 r3 r4)) (emptySpaces r1)
fromBufferToRoom06 _ = []
fromBufferToRoom07 (House (Buffer a b N N N N A) r1 r2 r3 r4) = map (\i -> (cost (9+i) A, House (Buffer a b N N N N N) (r1 A.// [(i, A)]) r2 r3 r4)) (emptySpaces r1)
fromBufferToRoom07 _ = []
fromBufferToRoom08 (House (Buffer B N N d e f g) r1 r2 r3 r4) = map (\i -> (cost (5+i) B, House (Buffer N N N d e f g) r1 (r2 A.// [(i, B)]) r3 r4)) (emptySpaces r2)
fromBufferToRoom08 _ = []
fromBufferToRoom09 (House (Buffer a B N d e f g) r1 r2 r3 r4) = map (\i -> (cost (4+i) B, House (Buffer a N N d e f g) r1 (r2 A.// [(i, B)]) r3 r4)) (emptySpaces r2)
fromBufferToRoom09 _ = []
fromBufferToRoom10 (House (Buffer a b B d e f g) r1 r2 r3 r4) = map (\i -> (cost (2+i) B, House (Buffer a b N d e f g) r1 (r2 A.// [(i, B)]) r3 r4)) (emptySpaces r2)
fromBufferToRoom10 _ = []
fromBufferToRoom11 (House (Buffer a b c B e f g) r1 r2 r3 r4) = map (\i -> (cost (2+i) B, House (Buffer a b c N e f g) r1 (r2 A.// [(i, B)]) r3 r4)) (emptySpaces r2)
fromBufferToRoom11 _ = []
fromBufferToRoom12 (House (Buffer a b c N B f g) r1 r2 r3 r4) = map (\i -> (cost (4+i) B, House (Buffer a b c N N f g) r1 (r2 A.// [(i, B)]) r3 r4)) (emptySpaces r2)
fromBufferToRoom12 _ = []
fromBufferToRoom13 (House (Buffer a b c N N B g) r1 r2 r3 r4) = map (\i -> (cost (6+i) B, House (Buffer a b c N N N g) r1 (r2 A.// [(i, B)]) r3 r4)) (emptySpaces r2)
fromBufferToRoom13 _ = []
fromBufferToRoom14 (House (Buffer a b c N N N B) r1 r2 r3 r4) = map (\i -> (cost (7+i) B, House (Buffer a b c N N N N) r1 (r2 A.// [(i, B)]) r3 r4)) (emptySpaces r2)
fromBufferToRoom14 _ = []
fromBufferToRoom15 (House (Buffer C N N N e f g) r1 r2 r3 r4) = map (\i -> (cost (7+i) C, House (Buffer N N N N e f g) r1 r2 (r3 A.// [(i, C)]) r4)) (emptySpaces r3)
fromBufferToRoom15 _ = []
fromBufferToRoom16 (House (Buffer a C N N e f g) r1 r2 r3 r4) = map (\i -> (cost (6+i) C, House (Buffer a N N N e f g) r1 r2 (r3 A.// [(i, C)]) r4)) (emptySpaces r3)
fromBufferToRoom16 _ = []
fromBufferToRoom17 (House (Buffer a b C N e f g) r1 r2 r3 r4) = map (\i -> (cost (4+i) C, House (Buffer a b N N e f g) r1 r2 (r3 A.// [(i, C)]) r4)) (emptySpaces r3)
fromBufferToRoom17 _ = []
fromBufferToRoom18 (House (Buffer a b c C e f g) r1 r2 r3 r4) = map (\i -> (cost (2+i) C, House (Buffer a b c N e f g) r1 r2 (r3 A.// [(i, C)]) r4)) (emptySpaces r3)
fromBufferToRoom18 _ = []
fromBufferToRoom19 (House (Buffer a b c d C f g) r1 r2 r3 r4) = map (\i -> (cost (2+i) C, House (Buffer a b c d N f g) r1 r2 (r3 A.// [(i, C)]) r4)) (emptySpaces r3)
fromBufferToRoom19 _ = []
fromBufferToRoom20 (House (Buffer a b c d N C g) r1 r2 r3 r4) = map (\i -> (cost (4+i) C, House (Buffer a b c d N N g) r1 r2 (r3 A.// [(i, C)]) r4)) (emptySpaces r3)
fromBufferToRoom20 _ = []
fromBufferToRoom21 (House (Buffer a b c d N N C) r1 r2 r3 r4) = map (\i -> (cost (5+i) C, House (Buffer a b c d N N N) r1 r2 (r3 A.// [(i, C)]) r4)) (emptySpaces r3)
fromBufferToRoom21 _ = []
fromBufferToRoom22 (House (Buffer D N N N N f g) r1 r2 r3 r4) = map (\i -> (cost (9+i) D, House (Buffer N N N N N f g) r1 r2 r3 (r4 A.// [(i, D)]))) (emptySpaces r4)
fromBufferToRoom22 _ = []
fromBufferToRoom23 (House (Buffer a D N N N f g) r1 r2 r3 r4) = map (\i -> (cost (8+i) D, House (Buffer a N N N N f g) r1 r2 r3 (r4 A.// [(i, D)]))) (emptySpaces r4)
fromBufferToRoom23 _ = []
fromBufferToRoom24 (House (Buffer a b D N N f g) r1 r2 r3 r4) = map (\i -> (cost (6+i) D, House (Buffer a b N N N f g) r1 r2 r3 (r4 A.// [(i, D)]))) (emptySpaces r4)
fromBufferToRoom24 _ = []
fromBufferToRoom25 (House (Buffer a b c D N f g) r1 r2 r3 r4) = map (\i -> (cost (4+i) D, House (Buffer a b c N N f g) r1 r2 r3 (r4 A.// [(i, D)]))) (emptySpaces r4)
fromBufferToRoom25 _ = []
fromBufferToRoom26 (House (Buffer a b c d D f g) r1 r2 r3 r4) = map (\i -> (cost (2+i) D, House (Buffer a b c d N f g) r1 r2 r3 (r4 A.// [(i, D)]))) (emptySpaces r4)
fromBufferToRoom26 _ = []
fromBufferToRoom27 (House (Buffer a b c d e D g) r1 r2 r3 r4) = map (\i -> (cost (2+i) D, House (Buffer a b c d e N g) r1 r2 r3 (r4 A.// [(i, D)]))) (emptySpaces r4)
fromBufferToRoom27 _ = []
fromBufferToRoom28 (House (Buffer a b c d e N D) r1 r2 r3 r4) = map (\i -> (cost (3+i) D, House (Buffer a b c d e N N) r1 r2 r3 (r4 A.// [(i, D)]))) (emptySpaces r4)
fromBufferToRoom28 _ = []

fromBufferToRoom :: [House -> [(Int, House)]]
fromBufferToRoom = [fromBufferToRoom01, fromBufferToRoom02, fromBufferToRoom03, fromBufferToRoom04, fromBufferToRoom05, fromBufferToRoom06, fromBufferToRoom07, fromBufferToRoom08, fromBufferToRoom09, fromBufferToRoom10, fromBufferToRoom11, fromBufferToRoom12, fromBufferToRoom13, fromBufferToRoom14, fromBufferToRoom15, fromBufferToRoom16, fromBufferToRoom17, fromBufferToRoom18, fromBufferToRoom19, fromBufferToRoom20, fromBufferToRoom21, fromBufferToRoom22, fromBufferToRoom23, fromBufferToRoom24, fromBufferToRoom25, fromBufferToRoom26, fromBufferToRoom27, fromBufferToRoom28]

firstInRoom :: A.Array Int Cell -> Maybe Int
firstInRoom arr = listToMaybe $ dropWhile (\i -> arr A.! i == N) $ A.indices arr

maybeUpdateHouse :: Maybe Int -> (Int -> (Int, House)) -> Maybe (Int, House)
maybeUpdateHouse Nothing _ = Nothing
maybeUpdateHouse (Just x) f = Just $ f x

fromRoomToBuffer01 (House (Buffer N N c d e f g) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r1) (\i -> (cost (3+i) (r1 A.! i), House (Buffer (r1 A.! i) N c d e f g) (r1 A.// [(i, N)]) r2 r3 r4))
fromRoomToBuffer01 _ = Nothing
fromRoomToBuffer02 (House (Buffer a N c d e f g) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r1) (\i -> (cost (2+i) (r1 A.! i), House (Buffer a (r1 A.! i) c d e f g) (r1 A.// [(i, N)]) r2 r3 r4))
fromRoomToBuffer02 _ = Nothing
fromRoomToBuffer03 (House (Buffer a b N d e f g) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r1) (\i -> (cost (2+i) (r1 A.! i), House (Buffer a b (r1 A.! i) d e f g) (r1 A.// [(i, N)]) r2 r3 r4))
fromRoomToBuffer03 _ = Nothing
fromRoomToBuffer04 (House (Buffer a b N N e f g) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r1) (\i -> (cost (4+i) (r1 A.! i), House (Buffer a b N (r1 A.! i) e f g) (r1 A.// [(i, N)]) r2 r3 r4))
fromRoomToBuffer04 _ = Nothing
fromRoomToBuffer05 (House (Buffer a b N N N f g) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r1) (\i -> (cost (6+i) (r1 A.! i), House (Buffer a b N N (r1 A.! i) f g) (r1 A.// [(i, N)]) r2 r3 r4))
fromRoomToBuffer05 _ = Nothing
fromRoomToBuffer06 (House (Buffer a b N N N N g) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r1) (\i -> (cost (8+i) (r1 A.! i), House (Buffer a b N N N (r1 A.! i) g) (r1 A.// [(i, N)]) r2 r3 r4))
fromRoomToBuffer06 _ = Nothing
fromRoomToBuffer07 (House (Buffer a b N N N N N) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r1) (\i -> (cost (9+i) (r1 A.! i), House (Buffer a b N N N N (r1 A.! i)) (r1 A.// [(i, N)]) r2 r3 r4))
fromRoomToBuffer07 _ = Nothing
fromRoomToBuffer08 (House (Buffer N N N d e f g) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r2) (\i -> (cost (5+i) (r2 A.! i), House (Buffer (r2 A.! i) N N d e f g) r1 (r2 A.// [(i, N)]) r3 r4))
fromRoomToBuffer08 _ = Nothing
fromRoomToBuffer09 (House (Buffer a N N d e f g) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r2) (\i -> (cost (4+i) (r2 A.! i), House (Buffer a (r2 A.! i) N d e f g) r1 (r2 A.// [(i, N)]) r3 r4))
fromRoomToBuffer09 _ = Nothing
fromRoomToBuffer10 (House (Buffer a b N d e f g) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r2) (\i -> (cost (2+i) (r2 A.! i), House (Buffer a b (r2 A.! i) d e f g) r1 (r2 A.// [(i, N)]) r3 r4))
fromRoomToBuffer10 _ = Nothing
fromRoomToBuffer11 (House (Buffer a b c N e f g) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r2) (\i -> (cost (2+i) (r2 A.! i), House (Buffer a b c (r2 A.! i) e f g) r1 (r2 A.// [(i, N)]) r3 r4))
fromRoomToBuffer11 _ = Nothing
fromRoomToBuffer12 (House (Buffer a b c N N f g) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r2) (\i -> (cost (4+i) (r2 A.! i), House (Buffer a b c N (r2 A.! i) f g) r1 (r2 A.// [(i, N)]) r3 r4))
fromRoomToBuffer12 _ = Nothing
fromRoomToBuffer13 (House (Buffer a b c N N N g) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r2) (\i -> (cost (6+i) (r2 A.! i), House (Buffer a b c N N (r2 A.! i) g) r1 (r2 A.// [(i, N)]) r3 r4))
fromRoomToBuffer13 _ = Nothing
fromRoomToBuffer14 (House (Buffer a b c N N N N) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r2) (\i -> (cost (7+i) (r2 A.! i), House (Buffer a b c N N N (r2 A.! i)) r1 (r2 A.// [(i, N)]) r3 r4))
fromRoomToBuffer14 _ = Nothing
fromRoomToBuffer15 (House (Buffer N N N N e f g) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r3) (\i -> (cost (7+i) (r3 A.! i), House (Buffer (r3 A.! i) N N N e f g) r1 r2 (r3 A.// [(i, N)]) r4))
fromRoomToBuffer15 _ = Nothing
fromRoomToBuffer16 (House (Buffer a N N N e f g) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r3) (\i -> (cost (6+i) (r3 A.! i), House (Buffer a (r3 A.! i) N N e f g) r1 r2 (r3 A.// [(i, N)]) r4))
fromRoomToBuffer16 _ = Nothing
fromRoomToBuffer17 (House (Buffer a b N N e f g) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r3) (\i -> (cost (4+i) (r3 A.! i), House (Buffer a b (r3 A.! i) N e f g) r1 r2 (r3 A.// [(i, N)]) r4))
fromRoomToBuffer17 _ = Nothing
fromRoomToBuffer18 (House (Buffer a b c N e f g) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r3) (\i -> (cost (2+i) (r3 A.! i), House (Buffer a b c (r3 A.! i) e f g) r1 r2 (r3 A.// [(i, N)]) r4))
fromRoomToBuffer18 _ = Nothing
fromRoomToBuffer19 (House (Buffer a b c d N f g) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r3) (\i -> (cost (2+i) (r3 A.! i), House (Buffer a b c d (r3 A.! i) f g) r1 r2 (r3 A.// [(i, N)]) r4))
fromRoomToBuffer19 _ = Nothing
fromRoomToBuffer20 (House (Buffer a b c d N N g) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r3) (\i -> (cost (4+i) (r3 A.! i), House (Buffer a b c d N (r3 A.! i) g) r1 r2 (r3 A.// [(i, N)]) r4))
fromRoomToBuffer20 _ = Nothing
fromRoomToBuffer21 (House (Buffer a b c d N N N) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r3) (\i -> (cost (5+i) (r3 A.! i), House (Buffer a b c d N N (r3 A.! i)) r1 r2 (r3 A.// [(i, N)]) r4))
fromRoomToBuffer21 _ = Nothing
fromRoomToBuffer22 (House (Buffer N N N N N f g) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r4) (\i -> (cost (9+i) (r4 A.! i), House (Buffer (r4 A.! i) N N N N f g) r1 r2 r3 (r4 A.// [(i, N)])))
fromRoomToBuffer22 _ = Nothing
fromRoomToBuffer23 (House (Buffer a N N N N f g) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r4) (\i -> (cost (8+i) (r4 A.! i), House (Buffer a (r4 A.! i) N N N f g) r1 r2 r3 (r4 A.// [(i, N)])))
fromRoomToBuffer23 _ = Nothing
fromRoomToBuffer24 (House (Buffer a b N N N f g) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r4) (\i -> (cost (6+i) (r4 A.! i), House (Buffer a b (r4 A.! i) N N f g) r1 r2 r3 (r4 A.// [(i, N)])))
fromRoomToBuffer24 _ = Nothing
fromRoomToBuffer25 (House (Buffer a b c N N f g) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r4) (\i -> (cost (4+i) (r4 A.! i), House (Buffer a b c (r4 A.! i) N f g) r1 r2 r3 (r4 A.// [(i, N)])))
fromRoomToBuffer25 _ = Nothing
fromRoomToBuffer26 (House (Buffer a b c d N f g) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r4) (\i -> (cost (2+i) (r4 A.! i), House (Buffer a b c d (r4 A.! i) f g) r1 r2 r3 (r4 A.// [(i, N)])))
fromRoomToBuffer26 _ = Nothing
fromRoomToBuffer27 (House (Buffer a b c d e N g) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r4) (\i -> (cost (2+i) (r4 A.! i), House (Buffer a b c d e (r4 A.! i) g) r1 r2 r3 (r4 A.// [(i, N)])))
fromRoomToBuffer27 _ = Nothing
fromRoomToBuffer28 (House (Buffer a b c d e N N) r1 r2 r3 r4) = maybeUpdateHouse (firstInRoom r4) (\i -> (cost (3+i) (r4 A.! i), House (Buffer a b c d e N (r4 A.! i)) r1 r2 r3 (r4 A.// [(i, N)])))
fromRoomToBuffer28 _ = Nothing

fromRoomToBuffer :: [House -> Maybe (Int, House)]
fromRoomToBuffer = [fromRoomToBuffer01, fromRoomToBuffer02, fromRoomToBuffer03, fromRoomToBuffer04, fromRoomToBuffer05, fromRoomToBuffer06, fromRoomToBuffer07, fromRoomToBuffer08, fromRoomToBuffer09, fromRoomToBuffer10, fromRoomToBuffer11, fromRoomToBuffer12, fromRoomToBuffer13, fromRoomToBuffer14, fromRoomToBuffer15, fromRoomToBuffer16, fromRoomToBuffer17, fromRoomToBuffer18, fromRoomToBuffer19, fromRoomToBuffer20, fromRoomToBuffer21, fromRoomToBuffer22, fromRoomToBuffer23, fromRoomToBuffer24, fromRoomToBuffer25, fromRoomToBuffer26, fromRoomToBuffer27, fromRoomToBuffer28]

getNeighbors :: House -> [(Int, House)]
getNeighbors house = concatMap (\f -> f house) fromBufferToRoom ++ mapMaybe (\f -> f house) fromRoomToBuffer


answerF p dist _ = dist M.! p

targetF :: Int -> House -> House
targetF n _ = House emptyBuffer (mkConstRoom n A) (mkConstRoom n B) (mkConstRoom n C) (mkConstRoom n D)


solution n house = dijkstra house target getNeighbors (answerF target)
    where target = targetF n house


solve1 = solution 2 . mkHouse

solve2 = mkHouse

solve :: String -> String
solve xs = show (solve1 $ parseContent1 xs, solve2 $ parseContent2 xs)

