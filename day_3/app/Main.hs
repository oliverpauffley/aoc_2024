{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Data.Foldable
import           Data.Functor
import           Data.Maybe          (catMaybes)
import           Text.Trifecta

data Instruction = Mul (Int, Int) | Do | Don't
  deriving (Eq, Show)

mulParser :: Parser String
mulParser = string "mul"

mulExprParser :: Parser (Int, Int)
mulExprParser = do
  _ <- mulParser
  _ <- char '('
  first <- threeDigitParser
  _ <- char ','
  second <- threeDigitParser
  _ <- char ')'
  return (first, second)

threeDigitParser :: Parser Int
threeDigitParser = choice [try (nDigitParser 3), try (nDigitParser 2), try (nDigitParser 1)]

nDigitParser :: Int -> Parser Int
nDigitParser n = read <$> count n digit

part1Parser :: Parser [(Int, Int)]
part1Parser = catMaybes <$> some (go <* optional newline) <* eof
  where
    go =
      choice
        [ Just <$> try mulExprParser,
          anyChar $> Nothing <?> "nothing parse"
        ]
        <?> "choice parse"

doParser :: Parser Instruction
doParser = Do <$ symbol "do()"

don'tParser :: Parser Instruction
don'tParser = Don't <$ symbol "don't()"

part2Parser :: Parser [Instruction]
part2Parser = catMaybes <$> some (go <* optional newline) <* eof
  where
    go =
      choice
        [ Just . Mul <$> try mulExprParser,
          Just <$> try doParser,
          Just <$> try don'tParser,
          anyChar $> Nothing <?> "nothing parse"
        ]
        <?> "choice parse"

part2Fold :: [Instruction] -> Int
part2Fold [] = 0
part2Fold xs = go xs True 0
  where
    go :: [Instruction] -> Bool -> Int -> Int
    go [] _ acc = acc
    go (y : ys) isEnabled acc = case (y, isEnabled) of
      (Mul (a, b), True)  -> go ys isEnabled acc + (a * b)
      (Mul (_, _), False) -> go ys isEnabled acc
      (Do, _)             -> go ys True acc
      (Don't, _)          -> go ys False acc

main :: IO ()
main = do
  result <- parseFromFile part1Parser "day_3/input/day_3.txt"
  case result of
    Nothing -> putStrLn "couldn't parse"
    Just a -> do
      print $ (sum . map (uncurry (*))) a

  result2 <- parseFromFile part2Parser "day_3/input/day_3.txt"
  case result2 of
    Nothing -> putStrLn "couldn't parse"
    Just a -> do
      print a
      print $ part2Fold a
