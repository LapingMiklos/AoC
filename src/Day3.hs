{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day3 where

import Data.Char (digitToInt, isDigit)
import Data.Maybe (isNothing, mapMaybe)
import Util (maybeAppend)

data Number = Num Integer Integer Integer deriving (Show)

addToNum :: Integer -> Number -> Number
addToNum x (Num y s e) = Num (y * 10 + x) s (e + 1)

-- processList :: String -> ([Number], [Integer])
-- processList str = processList' str [] Nothing [] 0
--   where
--     processList' [] nums currNum syms _ = (maybeAppend currNum nums, syms)
--     processList' (c : cs) nums currNum syms i
--       | c == '.' = processList' cs (maybeAppend currNum nums) Nothing syms (i + 1)
--       | isDigit c && isNothing currNum = processList' cs nums (Just (Num (toInteger $ digitToInt c) i i)) syms (i + 1)
--       | isDigit c = processList' cs nums (fmap (addToNum (toInteger $ digitToInt c)) currNum) syms (i + 1)
--       | otherwise = processList' cs (maybeAppend currNum nums) Nothing (i : syms) (i + 1)

-- isPart :: [Integer] -> Number -> Bool
-- isPart syms (Num _ s e) = any (\x -> x >= (s - 1) && x <= (e + 1)) syms

-- rowSumUp :: ([Number], [Integer]) -> Integer
-- rowSumUp (nums, syms) = sum $ map num $ filter (isPart syms) nums

-- addUp :: [([Number], [Integer])] -> Integer
-- addUp (x : y : xs) = rowSumUp (fst x, (snd x) ++ (snd y)) + (addUp' (x : y : xs))
--   where
--     addUp' [x, y] = rowSumUp (fst y, (snd x) ++ (snd y))
--     addUp' (x : y : z : xs) = rowSumUp (fst y, (snd x) ++ (snd y) ++ (snd z)) + (addUp' (y : z : xs))
--     addUp' _ = undefined
-- addUp _ = undefined

processListGears :: String -> ([Number], [Integer])
processListGears str = processList' str [] Nothing [] 0
  where
    processList' [] nums currNum syms _ = (maybeAppend currNum nums, syms)
    processList' (c : cs) nums currNum syms i
      | c == '*' = processList' cs (maybeAppend currNum nums) Nothing (i : syms) (i + 1)
      | isDigit c && isNothing currNum = processList' cs nums (Just (Num (toInteger $ digitToInt c) i i)) syms (i + 1)
      | isDigit c = processList' cs nums (fmap (addToNum (toInteger $ digitToInt c)) currNum) syms (i + 1)
      | otherwise = processList' cs (maybeAppend currNum nums) Nothing syms (i + 1)

returnIfAdjacent :: Integer -> Number -> Maybe Integer
returnIfAdjacent gear (Num n s e) =
  if (gear - 1 <= e) && (s <= gear + 1)
    then Just n
    else Nothing

gearRatio :: [Integer] -> Integer
gearRatio [x, y] = x * y
gearRatio _ = 0

rowSumUpGear :: ([Number], [Integer]) -> Integer
rowSumUpGear (nums, gears) = sum $ map (\gear -> gearRatio $ mapMaybe (returnIfAdjacent gear) nums) gears

addUpGear :: [([Number], [Integer])] -> Integer
addUpGear (x : y : xs) = rowSumUpGear ((fst x) ++ (fst y), snd x) + (addUp' (x : y : xs))
  where
    addUp' [x, y] = rowSumUpGear (fst x ++ fst y, snd y)
    addUp' (x : y : z : xs) = rowSumUpGear (fst x ++ fst y ++ fst z, snd y) + (addUp' (y : z : xs))
    addUp' _ = undefined
addUpGear _ = undefined

main :: IO ()
main = do
  content <- fmap lines $ readFile "res/d3/input.txt"
  -- print $ addUp $ map processList content
  let rows = map processListGears content
  mapM_ print rows
  print $ addUpGear $ rows