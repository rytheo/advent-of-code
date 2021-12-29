import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Maybe as Maybe

-- | Attempt to autocomplete a string of brackets.
-- Return a corrupt score (Left x) or completion score (Right x).
autoComplete :: String -> Either Int Int
autoComplete chars = do
    -- Create a stack of unclosed brackets
    stack <- Monad.foldM (\stack c ->
        if c `elem` "([{<"
        then Right $ c:stack
        else case stack of
            -- Check matching brackets by comparing ASCII value difference
            o:rest | o < c && Char.ord c - Char.ord o < 3 -> Right rest
            _ -> Left $ case c of
                ')' -> 3
                ']' -> 57
                '}' -> 1197
                '>' -> 25137
                _ -> error "Invalid char"
        ) [] chars
    Right $ foldl (\acc c -> 5 * acc + 1 + Maybe.fromJust (c `List.elemIndex` "([{<")) 0 stack

main :: IO ()
main = do
    text <- readFile "../../input/2021/input_10.txt"
    let (corruptScores, autoScores) = Either.partitionEithers $ autoComplete <$> lines text
    putStrLn $ "Part 1: " ++ show (sum corruptScores)
    let sorted = List.sort autoScores
    putStrLn $ "Part 2: " ++ show (sorted !! (length sorted `div` 2))
