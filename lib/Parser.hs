{-# LANGUAGE InstanceSigs #-}

module Parser
  ( Parser (..),
    notNull,
    charPredicateParser,
    charParser,
    stringParser,
    stringLiteralParser,
    wsParser,
    charWsParser,
    intParser,
    spanParser,
    spanListParser,
  )
where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Tuple
import Utils

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

-- | Parses characters one by one into a string as long as predicate for rest of the string returns True
--
-- >>> runParser (spanListParser (/="end")) "beforeend"
-- Just ("end","before")
spanListParser :: (String -> Bool) -> Parser String
spanListParser f = Parser (Just . swap . spanList f)

-- | Parses characters one by one into a string as long as predicate for character returns True
--
-- >>> runParser (spanParser (/='>')) "before>after"
-- Just (">after","before")
spanParser :: (Char -> Bool) -> Parser String
spanParser f = Parser (Just . swap . span f)

-- | Parses character that can optionally be surrounded by whitespace
--
-- >>> runParser (charWsParser 'g') "    g      p"
-- Just ("p","    g      ")
charWsParser :: Char -> Parser String
charWsParser c =
  (\x y z -> x ++ y : z)
    <$> wsParser
    <*> charParser c
    <*> wsParser

-- | Parses whitespace
--
-- >>> runParser wsParser "    after"
-- Just ("after","    ")
wsParser :: Parser String
wsParser = spanParser isSpace

-- | Parses string literal surrounded by double quotes
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

-- | Wraps a list parser into the parser that makes sure that the parsed list is not empty
--
-- >>> runParser (notNull (pure ["a"])) ""
-- Just ("",["a"])
--
-- >>> runParser (notNull (pure [])) ""
-- Nothing
notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser (p >=> f)
  where
    f x | (null . snd) x = Nothing
    f x = Just x