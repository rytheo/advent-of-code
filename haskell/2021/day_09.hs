import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

type Point = (Int, Int)

-- | Return a list of points adjacent to the given one.
findAdjacent :: Point -> Map.Map Point Int -> [Point]
findAdjacent (y, x) grid = filter (`Map.member` grid)
    [(y + dy, x + dx) | (dy, dx) <- [(0, 1), (1, 0), (0, -1), (-1, 0)]]

-- | Return a list of points that are lower than all their adjacent points.
lowPoints :: Map.Map Point Int -> [Point]
lowPoints grid = [point
    | (point, height) <- Map.assocs grid,
    and [height < grid Map.! adj | adj <- findAdjacent point grid]]

-- | Return a set of points that will flow downward to the given point.
findBasin :: Point -> Map.Map Point Int -> Set.Set Point
findBasin base grid =
    let height = grid Map.! base
        upper = [p | p <- findAdjacent base grid, grid Map.! p > height]
    in foldl Set.union (Set.singleton base) [findBasin p grid | p <- upper, grid Map.! p < 9]

main :: IO ()
main = do
    text <- readFile "../../input/2021/input_09.txt"
    let grid = Map.fromAscList $ do
        (y, line) <- zip [0..] $ lines text
        (x, c) <- zip [0..] line
        return ((y, x), Char.digitToInt c)
    putStrLn $ "Part 1: " ++ show (sum [grid Map.! p + 1 | p <- lowPoints grid])
    let sizes = Set.size <$> [findBasin p grid | p <- lowPoints grid]
    putStrLn $ "Part 2: " ++ show (product $ take 3 $ List.sortBy (flip compare) sizes)
