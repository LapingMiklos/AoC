module Day1 where

import Control.Applicative
import Data.Char (isDigit)
import Data.Maybe (fromMaybe, mapMaybe)
import Parser

digitFilterP :: Parser (Maybe Int)
digitFilterP = (Just <$> digitP) <|> (const Nothing <$> charPredP (not . isDigit))

digitListP :: Parser [Int]
digitListP = mapMaybe id <$> many digitFilterP

stringAsDigitP :: (String -> Parser String) -> Parser Int
stringAsDigitP sp = foldl (<|>) empty $ map (\(s, i) -> const i <$> sp s) mappings
  where
    mappings = [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)]

digitStringsAndDigitsP :: Parser [Int]
digitStringsAndDigitsP = mapMaybe id <$> many (Just <$> stringAsDigitP keeperStringP <|> digitFilterP)

findFirstAndLastDigit :: Parser [Int] -> String -> Int
findFirstAndLastDigit parser str = 10 * (head nums) + (last nums)
  where
    nums = fromMaybe [0] $ snd <$> runParser parser str

main :: IO ()
main = do
  content <- lines <$> readFile "res/Y2023/input1.txt"
  print $ sum $ map (findFirstAndLastDigit digitListP) content
  print $ sum $ map (findFirstAndLastDigit digitStringsAndDigitsP) content