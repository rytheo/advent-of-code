import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.List.Split as Split
import qualified Data.Map as Map
import qualified Data.Set as Set

type Pair = (Char, Char)

-- | Perform one step of the simulation.
step :: Map.Map Pair Char -> Map.Map Pair Int -> Map.Map Pair Int
step ruleMap pairs = Map.fromListWith (+) $ do
    (pair@(a, b), n) <- Map.assocs pairs
    let c = ruleMap Map.! pair
    [((a, c), n), ((c, b), n)]

-- | Perform polymer expansion and return the difference between
-- most and least common elements.
simulate:: Map.Map Pair Char -> String -> Int -> Int
simulate ruleMap template steps =
    let pairs = zip template $ tail template
        initial = Map.fromListWith (+) $ zip pairs (repeat 1)
        final = iterate (step ruleMap) initial !! steps
        -- Count elements using pair counts
        elements = Map.fromListWith (+) $ do
            ((a, b), n) <- Map.assocs final
            [(a, n), (b, n)]
        -- Divide counts by 2; add 1 for the start and end
        fixed = Map.mapWithKey (\k v ->
            let extra = length $ filter (==k) [head template, last template]
            in v `div` 2 + extra
            ) elements
    in maximum (Map.elems fixed) - minimum (Map.elems fixed)

main :: IO ()
main = do
    text <- readFile "../../input/2021/input_14.txt"
    let template:_:rules = lines text
    let ruleMap = Map.fromList [((a, b), c) | a:b:_:_:_:_:c:_ <- rules]
    putStrLn $ "Part 1: " ++ show (simulate ruleMap template 10)
    putStrLn $ "Part 2: " ++ show (simulate ruleMap template 40)
