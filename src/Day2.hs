module Day2 where

import Control.Applicative
import Data.Maybe (mapMaybe)
import Parser

data Round = Round Int Int Int deriving (Show)

data Color = R | G | B

instance Semigroup Round where
  (<>) (Round r1 g1 b1) (Round r2 g2 b2) = Round (max r1 r2) (max g1 g2) (max b1 b2)

instance Monoid Round where
  mempty = Round 0 0 0

data Game = Game {gameId :: Int, getRounds :: [Round]} deriving (Show)

colorParser :: Parser Color
colorParser = (const R <$> stringP "red") <|> (const G <$> stringP "green") <|> (const B <$> stringP "blue")

cToRound :: Color -> Int -> Round
cToRound R x = Round x 0 0
cToRound G x = Round 0 x 0
cToRound B x = Round 0 0 x

roundParser :: Parser Round
roundParser = foldMap (uncurry cToRound) <$> sepBy (charP ',') ((\_ n _ c -> (c, n)) <$> wsP <*> intP <*> wsP <*> colorParser)

gameParser :: Parser Game
gameParser = Game <$> (stringP "Game " *> intP <* charP ':') <*> sepBy (charP ';') roundParser

canFit :: Round -> Round -> Bool
canFit (Round r2 g2 b2) (Round r1 g1 b1) = r1 <= r2 && g1 <= g2 && b1 <= b2

power :: Round -> Int
power (Round r g b) = r * g * b

canGameFit :: Game -> Round -> Bool
canGameFit (Game _ rounds) round' = all (canFit round') rounds

gamePower :: Game -> Int
gamePower (Game _ rounds) = power $ foldl (<>) mempty rounds

main :: IO ()
main = do
  content <- lines <$> readFile "res/d2/input.txt"
  print $ sum $ map (\game -> if canGameFit game (Round 12 13 14) then gameId game else 0) $ games content
  print $ sum $ map gamePower $ games content
  where
    games = mapMaybe (fmap snd . runParser gameParser)