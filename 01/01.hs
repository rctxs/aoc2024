{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use bimap" #-}
import Data.List (foldl', sort)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  f1 <- readFile "input01.txt"
  f2 <- readFile "input02.txt"
  print $ taskOne f1
  print $ taskTwo f2

parse :: String -> ([Int], [Int])
parse = foldl' (\t lists -> (head lists : fst t, last lists : snd t)) ([], []) . map (map read . splitOn "   ") . lines

taskOne :: String -> Int
taskOne = sum . map (abs . uncurry (-)) . uncurry zip . (\(a, b) -> (sort a, sort b)) . parse

taskTwo :: String -> Int
taskTwo = sum . (\(a, b) -> ([a' * (length . filter (== a')) b | a' <- a])) . parse