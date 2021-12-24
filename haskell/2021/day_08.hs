import qualified Data.List.Split as Split
import qualified Data.Map as Map
import qualified Data.Set as Set

countEasy :: String -> Int
countEasy entry =
    let _:output:_ = Split.splitOn " | " entry
    in length [w | w <- words output, length w `elem` [2,4,3,7]]

parseEntry :: String -> Int
parseEntry entry =
    let input:output:_ = [Set.fromList <$> words s | s <- Split.splitOn " | " entry]
        setsOfLen n = [s | s <- input, Set.size s == n]
        -- Start with the signals of unique length
        one:_ = setsOfLen 2
        four:_ = setsOfLen 4
        seven:_ = setsOfLen 3
        eight:_ = setsOfLen 7
        -- Derive the rest
        adg = foldl1 Set.intersection $ setsOfLen 5
        three = Set.union one adg
        nine = Set.union four adg
        be = Set.difference eight three
        d = foldl1 Set.difference [four, one, be]
        zero = Set.difference eight d
        six:_ = [s | s <- setsOfLen 6, s `notElem` [zero, nine]]
        five = Set.intersection six nine
        ce = Set.difference eight five
        two = Set.union adg ce
        sigs = Map.fromList $ zip [zero, one, two, three, four, five, six, seven, eight, nine] [0..]
    in foldl (\acc s -> 10 * acc + (sigs Map.! s)) 0 output

main :: IO ()
main = do
    text <- readFile "../../input/2021/input_08.txt"
    putStrLn $ "Part 1: " ++ show (sum $ countEasy <$> lines text)
    putStrLn $ "Part 2: " ++ show (sum $ parseEntry <$> lines text)
