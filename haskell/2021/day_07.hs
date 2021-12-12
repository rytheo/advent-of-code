import qualified Data.List.Split as Split

minFuel :: Bool -> [Int] -> Int
minFuel constant crabs = minimum $ (\pos ->
        sum $ (\x -> let dist = abs (x - pos)
            in if constant then dist else dist * (dist + 1) `div` 2
        ) <$> crabs
    ) <$> [minimum crabs .. maximum crabs]

main :: IO ()
main = do
    text <- readFile "../../input/2021/input_07.txt"
    let crabs = read <$> Split.splitOn "," (head $ words text)
    putStrLn $ "Part 1: " ++ show (minFuel True crabs)
    putStrLn $ "Part 2: " ++ show (minFuel False crabs)
