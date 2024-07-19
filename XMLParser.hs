module XMLParser where

import Control.Applicative
import Data.Char
import Parser
import Utils

data XMLValue = XMLContent String | XMLNode String [(String, String)] [XMLValue] deriving (Show)

xmlNameParser :: Parser String
xmlNameParser =
  (:)
    <$> charPredicateParser isLetter
    <*> spanParser (\c -> isLetter c || isNumber c || c == '-' || c == '_' || c == '.')

attributeParser :: Parser (String, String)
attributeParser = do
  wsParser
  name <- xmlNameParser
  charWsParser '='
  value <- stringLiteralParser
  pure (name, value)

tagParser :: Parser (String, [(String, String)])
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

xmlContentParser :: Parser XMLValue
xmlContentParser =
  XMLContent . trim
    <$> notNull (spanParser (\c -> c /= '>' && c /= '<'))

xmlNodeParser :: Parser XMLValue
xmlNodeParser = do
  (tag, attributes) <- tagParser
  inner <- many xmlValueParser <|> pure []
  closingTagParser tag
  pure (XMLNode tag attributes inner)

xmlValueParser :: Parser XMLValue
xmlValueParser = xmlNodeParser <|> xmlContentParser

xmlHeaderParser :: Parser String
xmlHeaderParser =
  wsParser
    *> stringParser "<?"
    *> spanParser (/= '>')
    <* charParser '>'
    <* wsParser

xmlParser :: Parser XMLValue
xmlParser = do
  xmlHeaderParser <|> pure []
  xmlNodeParser