import qualified Data.List.Split as Split
import qualified Data.Map as Map

type Point = (Int, Int)
type Line = (Point, Point)

parseLine :: String -> Line
parseLine line =
    let x1:y1:x2:y2:_ = read <$> [s | s <- Split.splitOneOf ", ->" line, not (null s)]
    in ((x1, y1), (x2, y2))

isDiag :: Line -> Bool
isDiag ((x1, y1), (x2, y2)) = 0 `notElem` [x2 - x1, y2 - y1]

pointsFromLine :: Line -> [Point]
pointsFromLine ((x1, y1), (x2, y2)) =
    let [dx, dy] = (\x -> max (-1) (min x 1)) <$> [x2 - x1, y2 - y1]
    in zip [x1, x1+dx .. x2] [y1, y1+dy .. y2]

countIntersections :: Bool -> [Line] -> Int
countIntersections diagOk lineList =
    let points = concat $ pointsFromLine <$> filter (\line -> diagOk || not (isDiag line)) lineList
        grid = Map.fromListWith (+) $ zip points (repeat 1)
    in Map.size $ Map.filter (>1) grid

main :: IO ()
main = do
    text <- readFile "../../input/2021/input_05.txt"
    let lineList = parseLine <$> lines text
    putStrLn $ "Part 1: " ++ show (countIntersections False lineList)
    putStrLn $ "Part 2: " ++ show (countIntersections True lineList)
