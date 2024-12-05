{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Control.Monad
import           Data.String.QQ
import           Data.Text                  hiding (all, filter, head, length,
                                             map, tail)
import qualified Data.Text                  as T hiding (all, filter, head,
                                                  length, map, tail)
import           Data.Void
import           Text.Megaparsec            hiding (getInput, match)
import           Text.Megaparsec.Char       (char, digitChar, newline,
                                             numberChar)
import           Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void Text

data Rule = Rule Int Int
  deriving (Eq, Show)

data Input = Input [Rule] [[Int]]
  deriving (Eq, Show)

main :: IO ()
main = do
  sample <- getInput "day_5/input/sample.txt"
  input <- getInput "day_5/input/day_5.txt"
  putStr "Part 1 Sample: "
  print $ part1 sample

  putStr "Part 1: "
  print $ part1 input

  putStr "Part 2 Sample: "
  print $ part2 sample

  putStr "Part 2 : "
  print $ part2 input

getInput :: FilePath -> IO Input
getInput path = do
  file <- readFile path
  case runParser parseInput "" $ T.pack file of
    Left e  -> error $ "failed to parse" ++ show e
    Right i -> return i

parseInput :: Parser Input
parseInput = do
  rules <- parseRules
  prints <- parsePrints
  return $ Input rules prints

parseRule :: Parser Rule
parseRule = do
  from <- decimal
  _ <- char '|'
  to <- decimal
  return $ Rule from to

parseRules :: Parser [Rule]
parseRules = many (parseRule <* newline) <* newline

parsePrints :: Parser [[Int]]
parsePrints = many (some (decimal <* optional (char ',')) <* (void newline <|> eof))

-- >>> valid (Rule 47 53) 47 53
-- True

-- >>> valid (Rule 47 53) 53 47
-- False
valid :: Rule -> Int -> Int -> Bool
valid (Rule first second) a b
  | first == a && second == b = True
  | otherwise = False

order :: Rule -> Int -> Int -> Ordering
order (Rule first second) a b
  | first == a && second == b = LT
  | first == b && second == a = GT
  | otherwise = EQ

orderPair :: [Rule] -> Int -> Int -> Ordering
orderPair rs a b = maybeHead [order r a b | r <- rs, match r a b]
  where
    maybeHead [] = EQ
    maybeHead xs = head xs

-- >>> match (Rule 47 54) 47 53
-- False

-- >>> match (Rule 47 54) 54 47
-- False

match :: Rule -> Int -> Int -> Bool
match (Rule first second) a b = first == a && second == b || first == b && second == a

-- | for every rule we have check if any are broken by the list of int
-- >>> correctPrint [1,2,3,4] [Rule 1 2, Rule 3 1]
correctPrint :: [Rule] -> [Int] -> Bool
correctPrint _ [] = True
correctPrint rs (x : xs) = and [valid r x b | r <- rs, b <- xs, match r x b] && correctPrint rs xs

correctPrints :: [Rule] -> [[Int]] -> [[Int]]
correctPrints rs = filter (correctPrint rs)

-- >>> middle [1,2,3,4,5]
middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

part1 :: Input -> Int
part1 (Input rules prints) =
  sum . map middle $ correctPrints rules prints

notCorrectPrints :: Input -> [[Int]]
notCorrectPrints (Input rs prints) = filter (not . correctPrint rs) prints

ordWith :: [Rule] -> [Int] -> [Int]
ordWith _ [] = []
ordWith rs (x : xs) = zs ++ [x] ++ ys
  where
    zs = ordWith rs [z | z <- xs, orderPair rs z x == EQ || orderPair rs z x == LT]
    ys = ordWith rs [y | y <- xs, orderPair rs y x == GT]

part2 :: Input -> Int
part2 input@(Input rules _) = sum $ map (middle . ordWith rules) (notCorrectPrints input)
