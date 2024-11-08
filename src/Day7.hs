module Day7 where

import Control.Applicative
import Data.List (sort, sortBy, sortOn)
import Data.Maybe
import Parser
import Util (histogram, safeLast)

data Card = WildCard | Card Int | J | Q | K | A deriving (Show, Eq, Ord)

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Show, Eq, Ord)

data Hand = Hand {getHandType :: HandType, getCards :: [Card], getBid :: Integer} deriving (Show, Eq, Ord)

handType :: [Card] -> HandType
handType cards = handType' cardHist
  where
    cardHist = reverse $ sortOn snd $ histogram cards
    handType' [(_, 5)] = FiveOfAKind
    handType' ((_, 4) : _) = FourOfAKind
    handType' [(_, 3), (_, 2)] = FullHouse
    handType' ((_, 3) : _) = ThreeOfAKind
    handType' [(_, 2), (_, 2), (_, 1)] = TwoPair
    handType' ((_, 2) : _) = OnePair
    handType' ((_, 1) : _) = HighCard
    handType' hand = error $ show hand

cardParser :: Parser Card
cardParser = Card <$> digitP <|> Card 10 <$ charP 'T' <|> J <$ charP 'J' <|> Q <$ charP 'Q' <|> K <$ charP 'K' <|> A <$ charP 'A'

handParser :: Parser Hand
handParser = (\cards bid -> Hand (handType cards) cards bid) <$> many cardParser <* wsP <*> integerP

winnings :: (Hand -> Hand -> Ordering) -> [Hand] -> Integer
winnings cmp hands = sum $ zipWith (*) (map getBid $ sortBy cmp hands) [1 ..]

mapCardTo :: Card -> Card -> Hand -> Hand
mapCardTo cFrom cTo (Hand _ cards bid) = Hand (handType cards') cards' bid
  where
    cards' = map (\c -> if c == cFrom then cTo else c) cards

getMostCommonCard :: Hand -> Card
getMostCommonCard (Hand _ cards _) = fst . (fromMaybe (WildCard, 5)) . safeLast . sortOn snd $ histogram $ filter (/= WildCard) cards

customOrder :: Hand -> Hand -> Ordering
customOrder hSelf@(Hand htSelf cSelf _) hOther@(Hand htOther cOther _)
  | whtSelf < whtOther = LT
  | whtSelf > whtOther = GT
  | otherwise = compare cSelf cOther
  where
    mostCommonSelf = getMostCommonCard hSelf
    mostCommonOther = getMostCommonCard hOther
    (Hand whtSelf _ _) = mapCardTo WildCard mostCommonSelf hSelf
    (Hand whtOther _ _) = mapCardTo WildCard mostCommonOther hOther

main :: IO ()
main = do
  content <- lines <$> readFile "res/input7.txt"
  let hands = mapMaybe (fmap snd . runParser handParser) content
  print $ winnings compare hands
  let wildHands = map (mapCardTo J WildCard) hands
  print $ winnings customOrder wildHands