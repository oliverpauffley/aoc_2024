module Main where

import           Data.Map (Map)
import qualified Data.Map as Map

-- STOLEN from haskell unfoldr

main :: IO ()
main = do
  input <- fetchInput "day_4/input/day_4.txt"
  putStrLn "Part 1: "
  print $ checkInput input
  putStrLn "Part 2: "
  print $ checkInput' input

type Input = Map (Int, Int) Char

fetchInput :: FilePath -> IO Input
fetchInput file = do
  txt <- readFile file
  let ls :: [String]
      ls = lines txt
  pure $
    Map.fromList [((i, j), c) | (i, l) <- zip [0 ..] ls, (j, c) <- zip [0 ..] l]

checkInput :: Input -> Int
checkInput input =
  length $ filter (checkCandidate input) (candidates input)

checkCandidate :: Input -> [(Int, Int)] -> Bool
checkCandidate input path = lookupCandidate input path == Just "XMAS"

lookupCandidate :: Input -> [(Int, Int)] -> Maybe String
lookupCandidate input = traverse (`Map.lookup` input)

candidates :: Input -> [[(Int, Int)]]
candidates input = concatMap candidatesFrom (Map.keys input)

-- >>> candidatesFrom (5,3)
-- [[(5,3),(4,2),(3,1),(2,0)],[(5,3),(4,3),(3,3),(2,3)],[(5,3),(4,4),(3,5),(2,6)],[(5,3),(5,2),(5,1),(5,0)],[(5,3),(5,4),(5,5),(5,6)],[(5,3),(6,2),(7,1),(8,0)],[(5,3),(6,3),(7,3),(8,3)],[(5,3),(6,4),(7,5),(8,6)]]
candidatesFrom :: (Int, Int) -> [[(Int, Int)]]
candidatesFrom start = map (goFrom 4 start) directions

-- >>> directions
-- [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]
directions :: [(Int, Int)]
directions = [(i, j) | i <- [-1 .. 1], j <- [-1 .. 1], not (i == 0 && j == 0)]

-- | goFrom takes n elements from a start position and moving in the direction given by the second tuple.
-- >>> goFrom 4 (0,0) (1,0)
-- [(0,0),(1,0),(2,0),(3,0)]
goFrom :: Int -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
goFrom n (i1, j1) (i2, j2) = take n $ iterate (.+. (i2, j2)) (i1, j1)

(.+.) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(i1, j1) .+. (i2, j2) = (i1 + i2, j1 + j2)

(.-.) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(i1, j1) .-. (i2, j2) = (i1 - i2, j1 - j2)

tripleAround :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
tripleAround start d = [start .-. d, start, start .+. d]

-- >>> directionPairs
-- [((-1,-1),(1,-1)),((-1,-1),(-1,1)),((1,1),(1,-1)),((1,1),(-1,1))]
directionPairs :: [((Int, Int), (Int, Int))]
directionPairs = [(d1, d2) | d1 <- [(-1, -1), (1, 1)], d2 <- [(1, -1), (-1, 1)]]

-- >>> xsFrom (5,3)
-- [[(6,4),(4,2),(4,4),(6,2)],[(6,4),(4,2),(6,2),(4,4)],[(4,2),(6,4),(4,4),(6,2)],[(4,2),(6,4),(6,2),(4,4)]]
xsFrom :: (Int, Int) -> [[(Int, Int)]]
xsFrom p = map (\(d1, d2) -> tripleAround p d1 ++ tripleAround p d2) directionPairs

xs :: Input -> [[(Int, Int)]]
xs input = concatMap xsFrom (Map.keys input)

checkInput' :: Input -> Int
checkInput' input =
  length $ filter (checkX input) (xs input)

checkX :: Input -> [(Int, Int)] -> Bool
checkX input path = lookupCandidate input path == Just "MASMAS"
