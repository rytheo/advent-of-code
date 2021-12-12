import qualified Data.List.Split as Split
import qualified Data.Map as Map

step :: Map.Map Int Int -> Map.Map Int Int
step = Map.fromListWith (+) . concatMap (\(timer, count) ->
        if timer > 0 then [(timer - 1, count)] else [(6, count), (8, count)]
    ) . Map.assocs

main :: IO ()
main = do
    text <- readFile "../../input/2021/input_06.txt"
    let timers = read <$> Split.splitOn "," (head $ words text)
    let fish = Map.fromListWith (+) $ zip timers (repeat 1)
    putStrLn $ "Part 1: " ++ show (sum $ iterate step fish !! 80)
    putStrLn $ "Part 2: " ++ show (sum $ iterate step fish !! 256)
