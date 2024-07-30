module Lib.Parser where

import Control.Applicative
import Data.Char
import Data.Tuple
import Lib.Utils

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

-- | Wraps a list parser into the parser that makes sure that the parsed list is not empty
--
-- >>> runParser (notNull (pure ["a"])) ""
-- Just ("",["a"])
-- >>> runParser (notNull (pure [])) ""
-- Nothing
notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    if null xs
      then Nothing
      else Just (input', xs)

-- | Parses a character that meets the provided predicate
--
-- >>> runParser (charPredicateParser (/='>')) "a>b"
-- Just (">b",'a')
--
-- >>> runParser (charPredicateParser (/='>')) ">ab"
-- Nothing
charPredicateParser :: (Char -> Bool) -> Parser Char
charPredicateParser p = Parser f
  where
    f (y : ys) | p y = Just (ys, y)
    f _ = Nothing

-- | Parses integer
--
-- >>> runParser (intParser) "25after"
-- Just ("after",25)
--
-- >>> runParser (charParser 'c') "after25"
-- Nothing
intParser :: Parser Int
intParser = read <$> notNull (spanParser isDigit)

-- | Parses provided character
--
-- >>> runParser (charParser 'c') "cafter"
-- Just ("after",'c')
--
-- >>> runParser (charParser 'c') "afterc"
-- Nothing
charParser :: Char -> Parser Char
charParser c = charPredicateParser (== c)

-- | Parses characters one by one into a string as long as predice for character returns True
--
-- >>> runParser (spanParser (/='>')) "before>after"
-- Just (">after","before")
spanParser :: (Char -> Bool) -> Parser String
spanParser f = Parser (Just . swap . span f)

-- | Parses characters one by one into a string as long as predice for rest of the string returns True
--
-- >>> runParser (spanListParser (/="end")) "beforeend"
-- Just ("end","before")
spanListParser :: (String -> Bool) -> Parser String
spanListParser f = Parser (Just . swap . spanList f)

-- | Parses whitespaces
--
-- >>> runParser wsParser "    after"
-- Just ("after","    ")
wsParser :: Parser String
wsParser = spanParser isSpace

-- | Parses character that can optionaly be surounded by whitespaces
--
-- >>> runParser (charWsParser 'g') "    g      p"
-- Just ("p","    g      ")
charWsParser :: Char -> Parser String
charWsParser c =
  (\a b c -> a ++ [b] ++ c)
    <$> wsParser
    <*> charParser c
    <*> wsParser

-- | Parses string literal surounded by double quotes
--
-- >>> runParser stringLiteralParser "\"str\""
-- Just ("","str")
--
-- >>> runParser stringLiteralParser "\"str"
-- Nothing
stringLiteralParser :: Parser String
stringLiteralParser =
  charParser '"'
    *> spanParser (/= '"')
    <* charParser '"'

-- | Parses provided string
--
-- >>> runParser (stringParser "string") "stringafter"
-- Just ("after","string")
--
-- >>> runParser (stringParser "after") "stringafter"
-- Nothing
stringParser :: String -> Parser String
stringParser = traverse charParser
