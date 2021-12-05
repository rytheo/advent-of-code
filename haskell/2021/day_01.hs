increases :: (Num a, Ord b) => [b] -> a
increases xs = sum [1 | (x, y) <- zip xs $ tail xs, x < y]

windows :: Num a => [a] -> [a]
windows xs = [a + b + c | (a, b, c) <- zip3 xs (tail xs) (tail $ tail xs)]

main :: IO ()
main = do
    text <- readFile "../../input/2021/input_01.txt"
    let vals = map read $ words text :: [Int]
    putStrLn $ "Part 1: " ++ show (increases vals)
    putStrLn $ "Part 2: " ++ show (increases $ windows vals)
