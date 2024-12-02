import Data.List.Split (splitOn)

main :: IO ()
main = do
  f1 <- readFile "input01.txt"
  f2 <- readFile "input02.txt"
  print $ taskOne f1
  print $ taskTwo f2

taskOne :: String -> Int
taskOne = length . filter (safe . pairs) . parse

taskTwo :: String -> Int
taskTwo = length . filter (any (safe . pairs) . toleratedVariations) . parse

parse :: String -> [[Int]]
parse = map (map read . splitOn " ") . lines

pairs :: [Int] -> [(Int, Int)]
pairs l = [(l !! a, l !! (a + 1)) | a <- [0 .. (length l - 2)]]

safe :: [(Int, Int)] -> Bool
safe l = (all (uncurry (<)) l || all (uncurry (>)) l) && all (\(a, b) -> abs (a - b) < 4) l

toleratedVariations :: [Int] -> [[Int]]
toleratedVariations l = l : [take i l ++ drop (i + 1) l | i <- [0 .. (length l - 1)]]
