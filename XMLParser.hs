module XMLParser where

import Control.Applicative
import Data.Char
import Data.List
import Parser

type Attribute = (String, String)

data XMLValue = XMLContent String | XMLNode String [Attribute] [XMLValue]

instance Show XMLValue where
  show :: XMLValue -> String
  show (XMLContent s) = s
  show (XMLNode tag attrs children) =
    "<" ++ tag ++ showAttributes attrs ++ ">" ++ intercalate "" (show <$> children) ++ "</" ++ tag ++ ">"
    where
      showAttributes [] = ""
      showAttributes attrs = " " ++ unwords ((\(k, v) -> k ++ "=\"" ++ v ++ "\"") <$> attrs)

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