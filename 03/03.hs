main :: IO ()
main = do
  f1 <- readFile "input01.txt"
  f2 <- readFile "input02.txt"
  print $ taskOne f1
  print $ taskTwo f2

taskOne :: [Char] -> Integer
taskOne f = sum . map (uncurry (*)) $ parse f 0 "" "" []

taskTwo :: String -> Integer
taskTwo f = sum . map (uncurry (*)) $ parse' f True 0 "" "" []

-- -- input -> state -> factor a acc -> factor b acc -> pairs acc -> pairs
parse :: String -> Int -> String -> String -> [(Integer, Integer)] -> [(Integer, Integer)]
parse [] _ _ _ pairs = reverse pairs
parse ('m' : xs) _ _ _ pairs = parse xs 1 "" "" pairs
parse ('u' : xs) 1 _ _ pairs = parse xs 2 "" "" pairs
parse ('l' : xs) 2 _ _ pairs = parse xs 3 "" "" pairs
parse ('(' : xs) 3 _ _ pairs = parse xs 4 "" "" pairs
parse (c : xs) 4 a _ pairs
  | c `elem` "0123456789" = parse xs 4 (a ++ [c]) "" pairs
  | c == ',' = parse xs 5 a "" pairs
  | otherwise = parse xs 0 "" "" pairs
parse (c : xs) 5 a b pairs
  | c `elem` "0123456789" = parse xs 5 a (b ++ [c]) pairs
  | c == ')' = parse xs 0 "" "" ((read a, read b) : pairs)
  | otherwise = parse xs 0 "" "" pairs
parse (x : xs) _ _ _ pairs = parse xs 0 "" "" pairs

parse' :: String -> Bool -> Int -> String -> String -> [(Integer, Integer)] -> [(Integer, Integer)]
parse' [] _ _ _ _ pairs = reverse pairs
parse' ('d' : xs) state _ _ _ pairs
  | take 3 xs == "o()" = parse' xs True 0 "" "" pairs
  | take 6 xs == "on't()" = parse' xs False 0 "" "" pairs
  | otherwise = parse' xs state 0 "" "" pairs
parse' ('m' : xs) True _ _ _ pairs = parse' xs True 1 "" "" pairs
parse' ('u' : xs) True 1 _ _ pairs = parse' xs True 2 "" "" pairs
parse' ('l' : xs) True 2 _ _ pairs = parse' xs True 3 "" "" pairs
parse' ('(' : xs) True 3 _ _ pairs = parse' xs True 4 "" "" pairs
parse' (c : xs) True 4 a _ pairs
  | c `elem` "0123456789" = parse' xs True 4 (a ++ [c]) "" pairs
  | c == ',' = parse' xs True 5 a "" pairs
  | otherwise = parse' xs True 0 "" "" pairs
parse' (c : xs) True 5 a b pairs
  | c `elem` "0123456789" = parse' xs True 5 a (b ++ [c]) pairs
  | c == ')' = parse' xs True 0 "" "" ((read a, read b) : pairs)
  | otherwise = parse' xs True 0 "" "" pairs
parse' (x : xs) state _ _ _ pairs = parse' xs state 0 "" "" pairs
