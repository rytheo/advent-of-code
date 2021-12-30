import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

type Point = (Int, Int)

-- | Add two energy levels.
-- If either is 0, return 0.
addEnergy :: Int -> Int -> Int
addEnergy a b = if a == 0 || b == 0 then 0 else a + b

-- | Advance the simulation by one step.
step :: Map.Map Point Int -> Map.Map Point Int
step grid = until (all (<=9)) (\grid -> Map.fromListWith addEnergy $ do
        ((y, x), energy) <- Map.assocs grid
        -- Any octopus with energy > 9 flashes and its energy becomes 0
        -- Nearby unflashed octopi have their energy incremented
        if energy > 9
        then ((y, x), 0) : do
            dy <- [-1..1]
            dx <- [-1..1]
            let adj = (y + dy, x + dx)
            Monad.guard $ adj /= (y, x) && adj `Map.member` grid
            return (adj, 1)
        else return ((y, x), energy)
    ) $ (+1) <$> grid

main :: IO ()
main = do
    text <- readFile "../../input/2021/input_11.txt"
    let grid = Map.fromList $ do
        (y, line) <- zip [0..] $ lines text
        (x, c) <- zip [0..] line
        return ((y, x), Char.digitToInt c)
    let flashCount = length $ filter (==0) $ concatMap Map.elems $ take 101 $ iterate step grid
    putStrLn $ "Part 1: " ++ show flashCount
    let syncStep = Maybe.fromJust
            $ List.findIndex ((== Map.size grid) . Map.size . Map.filter (==0))
            $ iterate step grid
    putStrLn $ "Part 2: " ++ show syncStep
