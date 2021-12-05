module Day05 where
import Data.List.Split (splitOn)

type Point = (Int, Int)
type Line = (Point, Point)
type Board = [[Int]] -- optimization: Board can be a hashmap (x, y) -> count

parseLine :: String -> Line
parseLine line = ((x1, y1), (x2, y2))
    where [start, end] = splitOn "->" line
          [x1, y1] = map read $ splitOn "," start
          [x2, y2] = map read $ splitOn "," end

parseContent :: String -> [Line]
parseContent = map parseLine . lines

myListComp :: Int -> Int -> [Int]
myListComp x y
    | x < y = [x..y]
    | otherwise = reverse [y..x]

emptyBoard :: Int -> Int -> Board
emptyBoard n m = replicate n $ replicate m 0

mark :: Board -> Point -> Board
mark b (x, y) = up ++ (left ++ (a+1) : right) : down
    where (up, l:down) = splitAt y b
          (left, a:right) = splitAt x l

marks :: Board -> [Point] -> Board
marks = foldl mark

segmentToPoints :: Line -> [Point]
segmentToPoints ((x1, y1), (x2, y2))
    | x1 == x2 = [(x1, y) | y <- [min y1 y2 .. max y1 y2]]
    | y1 == y2 = [(x, y1) | x <- [min x1 x2 .. max x1 x2]]
    | otherwise = []

segmentToPoints' :: Line -> [Point]
segmentToPoints' ((x1, y1), (x2, y2))
    | x1 == x2 = [(x1, y) | y <- [min y1 y2 .. max y1 y2]]
    | y1 == y2 = [(x, y1) | x <- [min x1 x2 .. max x1 x2]]
    | abs (y2 - y1) == abs (x2 - x1) = zip (myListComp x1 x2) (myListComp y1 y2)
    | otherwise = []

getNM :: [Line] -> (Int, Int)
getNM ls = (m + 1, n + 1)
    where n = maximum $ concatMap (\(p1, p2) -> [fst p1, fst p2]) ls
          m = maximum $ concatMap (\(p1, p2) -> [snd p1, snd p2]) ls

solution1 :: [Line] -> Int
solution1 ls = result
    where ps = map segmentToPoints ls
          (n, m) = getNM ls
          b = foldl marks (emptyBoard n m) ps
          result = length $ filter (>=2) $ concat b

solution2 :: [Line] -> Int
solution2 ls = result
    where ps = map segmentToPoints' ls
          (n, m) = getNM ls
          b = foldl marks (emptyBoard n m) ps
          result = length $ filter (>=2) $ concat b

solve1 :: String -> Int
solve1 = solution1 . parseContent

solve2 :: String -> Int
solve2 = solution2 . parseContent

solve :: String -> IO ()
solve content = do
    print $ solve1 content
    print $ solve2 content

