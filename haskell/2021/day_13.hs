import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.List.Split as Split
import qualified Data.Map as Map
import qualified Data.Set as Set

type Point = (Int, Int)

foldPoint :: (Char, Int) -> Point -> Point
foldPoint (axis, val) (x, y)
    | axis == 'x' && val < x = (2 * val - x, y)
    | axis == 'y' && val < y = (x, 2 * val - y)
    | otherwise = (x, y)

main :: IO ()
main = do
    text <- readFile "../../input/2021/input_13.txt"
    -- Parse grid and instructions
    let points:orders:_ = Split.splitOn "\n\n" text
    let grid = Set.fromList $ do
        line <- lines points
        let x:y:_ = read <$> Split.splitOn "," line :: [Int]
        return (x, y)
    let folds = do
        line <- lines orders
        let token = last $ words line
        let (axis:_):val:_ = Split.splitOn "=" token
        return (axis, read val :: Int)
    -- Do the first fold
    let singleFold = Set.map (foldPoint $ head folds) grid
    putStrLn $ "Part 1: " ++ show (Set.size singleFold)
    -- Do the rest
    let folded = foldl (\g f -> Set.map (foldPoint f) g) grid folds
    let (xMax, _) = Set.findMax folded
    let yMax = maximum $ snd <$> Set.elems folded
    putStrLn "Part 2:"
    Monad.forM_ [0..yMax] $ \y -> do
        Monad.forM_ [0..xMax] $ \x ->
            putChar $ if (x, y) `Set.member` folded then '#' else ' '
        putChar '\n'
