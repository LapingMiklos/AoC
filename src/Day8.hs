module Day8 where

import Data.Char 
import Data.Map qualified as M
import Parser
import Control.Applicative
import Util 
import Data.Maybe


data Path = Path {getSteps :: [(String, String) -> String], getMap :: [(String, (String, String))]}

directionP :: Parser ((String, String) -> String)
directionP = fst <$ charP 'L' <|> snd <$ charP 'R'

choiceP :: Parser (String, (String, String))
choiceP = (\x y z -> (x, (y, z))) <$> spanP isAlphaNum <* stringP " = (" <*> spanP isAlphaNum <* stringP ", " <*> spanP isAlphaNum <* charP ')' <* wsP

pathP :: Parser Path
pathP = (\steps choices -> Path steps choices) <$> many directionP <* wsP <*> many (choiceP <* wsP)


walk :: Path -> Int
walk (Path steps choices) = walk' (repeat steps >>= id) (M.fromList choices) "AAA" "ZZZ"
    where
        walk' (st:sts) theMap start destination
            | start == destination = 0
            | otherwise = 1 + walk' sts theMap (st . unwrap $ M.lookup start theMap) destination

ghostWalk :: Path -> Int
ghostWalk (Path steps choices) = foldl lcm 1 $ map (ghostWalk' (repeat steps >>= id) (M.fromList choices)) $ mapMaybe (\c -> if last (fst c) == 'A' then Just (fst c) else Nothing) choices
  where
    ghostWalk' :: [(String, String) -> String] -> M.Map String (String, String) -> String -> Int
    ghostWalk' (st:sts) theMap start
      | last start == 'Z' = 0
      | otherwise = 1 + (ghostWalk' sts theMap $ st . unwrap $ M.lookup start theMap)

main :: IO ()
main = do
    content <- readFile "res/input8.txt"
    let path = snd . unwrap $ runParser pathP content
    print $ walk path
    print $ ghostWalk path