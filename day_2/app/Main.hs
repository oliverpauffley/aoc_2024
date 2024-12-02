module Main where

import           Data.List

main :: IO ()
main = do
  file <- readFile "day_2/input/day_2.txt"
  putStrLn "Part 1"
  print $ part1 file
  putStrLn "Part 2"
  print $ part2 file

part1 :: String -> Int
part1 = length . filter id . map (safeReport . lineToReports) . lines

lineToReports :: String -> [Int]
lineToReports = map read . words

safeReport :: [Int] -> Bool
safeReport xs = sorted xs && (adjacent . pairs) xs

sorted :: [Int] -> Bool
sorted xs = sort xs == xs || sort xs == reverse xs

pairs :: [Int] -> [(Int, Int)]
pairs xs = zip xs (tail xs)

adjacent :: [(Int, Int)] -> Bool
adjacent = all (\(x, y) -> difference x y >= 1 && difference x y < 4)
  where
    difference x y = abs (x - y)

part2 :: String -> Int
-- part2 = length . filter id . map (all safeReport) . (map dampener . lineToReports) . lines
part2 = length . filter id . map (any safeReport . dampener . lineToReports) . lines

-- | return all possible lists with one element removed
dampener :: [a] -> [[a]]
dampener [] = []
dampener xs = [take n xs ++ drop (n + 1) xs | n <- [0 .. length xs - 1]]
