run :: Num a => Bool -> [(String, a)] -> (a, a, a) -> a
run _ [] (_, x, y) = x * y
run adv ((dir, val):cmds) (aim, x, y) = run adv cmds $ case (adv, dir) of
    (False, "forward") -> (aim, x + val, y)
    (True, "forward") -> (aim, x + val, y + aim * val)
    (False, "down") -> (aim, x, y + val)
    (True, "down") -> (aim + val, x, y)
    (False, "up") -> (aim, x, y - val)
    (True, "up") -> (aim - val, x, y)
    _ -> error "Invalid input"

main :: IO ()
main = do
    text <- readFile "../../input/2021/input_02.txt"
    let cmds = [(s, read v :: Int) | s:v:_ <- map words $ lines text]
    putStrLn $ "Part 1: " ++ show (run False cmds (0, 0, 0))
    putStrLn $ "Part 2: " ++ show (run True cmds (0, 0, 0))
