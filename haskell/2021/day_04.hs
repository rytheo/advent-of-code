import qualified Data.List as List
import qualified Data.List.Split as Split

type Board = [[Int]]

readBoard :: String -> Board
readBoard s = [read <$> words line | line <- lines s]

mark :: Int -> Board -> Board
mark n = (map . map) (\b -> if b == n then -1 else b)

hasWon :: Board -> Bool
hasWon b = or [all (<0) r | r <- b ++ List.transpose b]

calcScore :: Int -> Board -> Int
calcScore n b = n * sum (filter (>=0) $ concat b)

play :: [Int] -> [Board] -> [Int]
play [] _ = []
play (num:nums) boards =
    let marked = mark num <$> boards
        (won, rest) = List.partition hasWon marked
    in (calcScore num <$> won) ++ play nums rest

main :: IO ()
main = do
    text <- readFile "../../input/2021/input_04.txt"
    let order:blocks = Split.splitOn "\n\n" text
    let nums = read <$> Split.splitOn "," order :: [Int]
    let boards = readBoard <$> blocks
    let scores = play nums boards
    putStrLn $ "Part 1: " ++ show (head scores)
    putStrLn $ "Part 2: " ++ show (last scores)
