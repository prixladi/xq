module XmlParser where

import Control.Applicative
import Data.Char
import Parser

type Attribute = (String, String)

data XmlValue = XmlContent String | XmlNode String [Attribute] [XmlValue] deriving (Show, Eq)

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

xmlNameParser :: Parser String
xmlNameParser =
  (:)
    <$> charPredicateParser isLetter
    <*> spanParser (\c -> isLetter c || isNumber c || c == '-' || c == '_' || c == '.')

attributeParser :: Parser Attribute
attributeParser = do
  wsParser
  name <- xmlNameParser
  charWsParser '='
  value <- stringLiteralParser
  pure (name, value)

tagParser :: Parser (String, [Attribute])
tagParser = do
  wsParser
  charParser '<'
  tag <- xmlNameParser
  attributes <- many attributeParser <|> pure []
  charWsParser '>'
  pure (tag, attributes)

closingTagParser :: String -> Parser String
closingTagParser a =
  wsParser
    *> stringParser "</"
    *> stringParser a
    <* charWsParser '>'

xmlContentParser :: Parser XmlValue
xmlContentParser =
  XmlContent . trim
    <$> notNull (spanParser (\c -> c /= '>' && c /= '<'))

xmlNodeParser :: Parser XmlValue
xmlNodeParser = do
  (tag, attributes) <- tagParser
  inner <- many xmlValueParser <|> pure []
  closingTagParser tag
  pure (XmlNode tag attributes inner)

xmlValueParser :: Parser XmlValue
xmlValueParser = xmlNodeParser <|> xmlContentParser

xmlHeaderParser :: Parser String
xmlHeaderParser =
  wsParser
    *> stringParser "<?"
    *> spanParser (/= '>')
    <* charParser '>'
    <* wsParser

xmlParser :: Parser XmlValue
xmlParser = do
  xmlHeaderParser <|> pure []
  xmlNodeParser