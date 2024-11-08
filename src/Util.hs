module Util where

import qualified Data.Text as T
import qualified Data.Text.Read as TR

toIntsUnsafe :: T.Text -> [Int]
toIntsUnsafe txt = map textToIntUnsafe $ filter (T.empty /=) $ T.split (' ' ==) txt

stringToIntUnsafe :: String -> Int
stringToIntUnsafe = textToIntUnsafe . T.pack

textToIntUnsafe :: T.Text -> Int
textToIntUnsafe = unwrap . textToInt

textToInt :: T.Text -> Maybe Int
textToInt text = case TR.decimal text of
  Right (n, _) -> Just n
  Left _ -> Nothing

unwrap :: Maybe a -> a
unwrap (Just a) = a
unwrap Nothing = error "Trying to unwrap Nothing"

maybeAppend :: Maybe a -> [a] -> [a]
maybeAppend Nothing xs = xs
maybeAppend (Just x) xs = (x : xs)

count :: (a -> Bool) -> [a] -> Int
count p = foldl (\n x -> if p x then n + 1 else n) 0

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x