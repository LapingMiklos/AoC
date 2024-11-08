module Day5 where

import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, empty, pack, split)
import Util (safeHead, toIntsUnsafe)

data Range = Range Int Int Int deriving (Show)

type DSMap = [Range]

rangeMap :: Int -> Range -> Maybe Int
rangeMap x (Range d s l)
  | x >= s && x < s + l = Just $ d + x - s
  | otherwise = Nothing

rangesMap :: DSMap -> Int -> Int
rangesMap rs x = fromMaybe x $ safeHead $ mapMaybe (rangeMap x) rs

navigateToEnd :: [DSMap] -> Int -> Int
navigateToEnd maps = foldl (flip (.)) id $ map rangesMap maps

toRange :: [Int] -> Range
toRange [d, s, l] = Range d s l
toRange _ = error "Invalid range"

parse :: [Text] -> [DSMap]
parse [] = []
parse rows = ranges : (parse $ drop 1 t)
  where
    (h, t) = span (empty /=) $ drop 1 rows
    ranges = map (toRange . toIntsUnsafe) h

flattenSeeds :: [Int] -> [Int]
flattenSeeds [] = []
flattenSeeds (x : y : xs) = [x .. (x + y)] ++ flattenSeeds xs
flattenSeeds _ = undefined

main :: IO ()
main = do
  content <- fmap lines $ readFile "res/d5/input.txt"
  let rows = map pack content
  let seeds = toIntsUnsafe $ (split (':' ==) $ head rows) !! 1
  let maps = parse $ drop 2 rows

  print $ minimum $ map (navigateToEnd maps) seeds

  let allSeeds = flattenSeeds seeds

  print $ minimum $ map (navigateToEnd maps) $ allSeeds
