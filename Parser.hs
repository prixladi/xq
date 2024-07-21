module Parser where

import Control.Applicative
import Data.Char
import Data.Tuple

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser (\input -> Just (input, x))

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      Just (input'', f a)

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser p1) >>= f =
    Parser $ \input -> do
      (input', a) <- p1 input
      runParser (f a) input'

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    if null xs
      then Nothing
      else Just (input', xs)

charPredicateParser :: (Char -> Bool) -> Parser Char
charPredicateParser p = Parser f
  where
    f (y : ys) | p y = Just (ys, y)
    f _ = Nothing

charParser :: Char -> Parser Char
charParser x = charPredicateParser (== x)

spanParser :: (Char -> Bool) -> Parser String
spanParser f = Parser (Just . swap . span f)

wsParser :: Parser String
wsParser = spanParser isSpace

charWsParser :: Char -> Parser String
charWsParser c =
  (\a b c -> a ++ [b] ++ c)
    <$> wsParser
    <*> charParser c
    <*> wsParser

stringLiteralParser :: Parser String
stringLiteralParser =
  charParser '"'
    *> spanParser (/= '"')
    <* charParser '"'

stringParser :: String -> Parser String
stringParser = traverse charParser
