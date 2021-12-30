import qualified Data.Char as Char
import qualified Data.List.Split as Split
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Return the number of paths from the current node to the end.
explore :: Bool -> Map.Map String (Set.Set String) -> Set.Set String -> String -> Int
explore _ _ _ "end" = 1
explore extraTime graph visited current = sum $ do
    adj <- Set.elems $ graph Map.! current
    -- Explore all large or unvisited caves
    if all Char.isUpper adj || adj `Set.notMember` visited
        then return $ explore extraTime graph (Set.insert adj visited) adj
    -- Explore one small cave twice
    else if extraTime && adj /= "start"
        then return $ explore False graph visited adj
    else []

main :: IO ()
main = do
    text <- readFile "../../input/2021/input_12.txt"
    let graph = Map.fromListWith Set.union $ do
        line <- lines text
        let a:b:_ = Split.splitOn "-" line
        (start, end) <- [(a, b), (b, a)]
        return (start, Set.singleton end)
    let visited = Set.singleton "start"
    putStrLn $ "Part 1: " ++ show (explore False graph visited "start")
    putStrLn $ "Part 1: " ++ show (explore True graph visited "start")
