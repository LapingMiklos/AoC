{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Day4 (main) where

import Data.Maybe
import Parser
import Util (count)

data Card = Card Int [Int] [Int] deriving (Show)

cardParser :: Parser Card
cardParser = Card <$> (stringP "Card" *> wsP *> intP <* charP ':') <*> sepBy wsP intP <* charP '|' <*> sepBy wsP intP

cardMatches :: Card -> Int
cardMatches (Card _ winning mine) = count (\x -> any (x ==) winning) mine

pointVal :: Int -> Int
pointVal 0 = 0
pointVal x = 2 ^ (x - 1)

cardCount :: [(Card, Int)] -> Int
cardCount [] = 0
cardCount ((card, c) : cs) = c + (cardCount $ addTo (cardMatches card) c cs)
  where
    addTo :: Int -> Int -> [(Card, Int)] -> [(Card, Int)]
    addTo _ _ [] = []
    addTo 0 _ cs = cs
    addTo i x ((card, c) : cs) = (card, c + x) : addTo (i - 1) x cs

main :: IO ()
main = do
  content <- fmap lines $ readFile "res/d4/input.txt"
  let cards = mapMaybe (fmap snd . runParser cardParser) content
  print $ sum $ map (pointVal . cardMatches) cards
  print $ cardCount $ zip cards $ repeat 1