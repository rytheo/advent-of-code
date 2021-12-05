import qualified Data.Char as Char

-- | Convert a list of bits into an integer.
binToInt :: [Int] -> Int
binToInt = foldl1 (\acc b -> 2 * acc + b)

-- | Map the inner elements from (0,1) to (-1,1),
-- then fold the lists via element-wise summation.
getDeltas :: [[Int]] -> [Int]
getDeltas = foldl1 (zipWith (+)) . (map . map) (\b -> b * 2 - 1)

-- | Compute the power consumption of the submarine.
findPower :: [[Int]] -> Int
findPower bins = gamma * epsilon
    where
    deltas = getDeltas bins
    gamma = binToInt [if d > 0 then 1 else 0 | d <- deltas]
    epsilon = binToInt [if d < 0 then 1 else 0 | d <- deltas]

-- | Compute the o2 (True) or co2 (False) rating from a list of binary strings.
findRating :: Bool -> Int -> [[Int]] -> Int
findRating _ _ [] = error "List is empty"
findRating _ _ [bin] = binToInt bin
findRating o2 i bins = findRating o2 (i + 1) $ filter predicate bins
    where
    deltas = getDeltas bins
    keep = if (deltas!!i >= 0) == o2 then 1 else 0
    predicate = \bin -> bin!!i == keep

main :: IO ()
main = do
    text <- readFile "../../input/2021/input_03.txt"
    let bins = (map . map) Char.digitToInt $ lines text
    putStrLn $ "Part 1: " ++ show (findPower bins)
    let oxygen = findRating True 0 bins
    let co2 = findRating False 0 bins
    putStrLn $ "Part 2: " ++ show (oxygen * co2)
