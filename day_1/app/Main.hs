module Main where

import           Data.List

main :: IO ()
main = do
  file <- readFile "day_1/input/day_1.txt"
  putStrLn "Part 1: "
  print $ part1 file
  putStrLn "Part 2: "
  print $ part2 file

part1 :: String -> Int
part1 = sum . map differences . zipper . sortBoth . unzip . map (groupLocations . words) . lines
  where
    sortBoth (xs, ys) = (sort xs, sort ys)
    zipper (xs, ys) = zip xs ys

groupLocations :: [String] -> (Int, Int)
groupLocations [x, y] = (read x, read y)
groupLocations _      = error "incorrect locations"

differences :: (Int, Int) -> Int
differences (x, y) = abs $ x - y

part2 :: String -> Int
part2 = similaritySum . unzip . map (groupLocations . words) . lines

similaritySum :: ([Int], [Int]) -> Int
similaritySum ([], _) = 0
similaritySum (x : xs, ys) = x * (length . filter (== x)) ys + similaritySum (xs, ys)
