module Parser where

import Control.Applicative
import Data.Char
import Data.Tuple

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (input', x) <- p input
    Just (input', f x)

instance Applicative Parser where
  pure p = Parser $ \input -> Just (input, p)
  Parser p1 <*> (Parser p2) = Parser $ \input -> do
    (input', f) <- p1 input
    (input'', x) <- p2 input'
    Just (input'', f x)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  Parser p1 <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

silence :: Monoid a => Parser a -> Parser a
silence (Parser p) = Parser $ \input -> do
  (input', _) <- p input
  Just (input', mempty)

charP :: Char -> Parser Char
charP c = Parser f
  where
    f (x : xs)
      | x == c = Just (xs, x)
      | otherwise = Nothing
    f _ = Nothing

charPredP :: (Char -> Bool) -> Parser Char
charPredP p = Parser f
  where
    f (x : xs)
      | p x = Just (xs, x)
      | otherwise = Nothing
    f _ = Nothing

digitP :: Parser Int
digitP = digitToInt <$> charPredP isDigit

notDigitP :: Parser String
notDigitP = spanP (not . isDigit)

integerP :: Parser Integer
integerP = read <$> spanP isDigit

intP :: Parser Int
intP = read <$> spanP isDigit

stringP :: String -> Parser String
stringP = sequenceA . map charP

spanP :: (Char -> Bool) -> Parser String
spanP p = Parser $ \input -> Just . swap $ match input
  where
    match input = span p input

wsP :: Parser String
wsP = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

keeperStringP :: String -> (Parser String)
keeperStringP [] = pure []
keeperStringP (x : xs) = Parser $ \input -> do
  (input', _) <- runParser (charP x) input
  _ <- runParser (stringP xs) input'
  Just (input', (x : xs))