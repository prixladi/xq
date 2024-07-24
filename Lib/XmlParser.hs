module Lib.XmlParser where

import Control.Applicative
import Data.Char
import Data.List
import Lib.Parser
import Lib.Utils

type Attribute = (String, String)

data XmlValue
  = XmlComment String
  | XmlContent String
  | XmlNode String [Attribute] [XmlValue]
  deriving (Show, Eq)

xmlNameParser :: Parser String
xmlNameParser =
  (:)
    <$> charPredicateParser isLetter
    <*> spanParser (\c -> isLetter c || isNumber c || c == '-' || c == '_' || c == '.')

attributeParser :: Parser Attribute
attributeParser =
  (\_ name _ value -> (name, value))
    <$> wsParser
    <*> xmlNameParser
    <*> charWsParser '='
    <*> stringLiteralParser

tagParser :: Parser (String, [Attribute])
tagParser =
  (\_ _ name attributes _ -> (name, attributes))
    <$> wsParser
    <*> charParser '<'
    <*> xmlNameParser
    <*> (many attributeParser <|> pure [])
    <*> charWsParser '>'

closingTagParser :: String -> Parser String
closingTagParser a =
  wsParser
    *> stringParser "</"
    *> stringParser a
    <* charWsParser '>'

xmlContentParser :: Parser XmlValue
xmlContentParser = XmlContent . trim <$> notNull (spanParser (/= '<'))

xmlCommentParser :: Parser XmlValue
xmlCommentParser = XmlComment <$> content
  where
    content = stringParser "<!--" *> spanListParser (not . isPrefixOf "-->") <* stringParser "-->"

xmlNodeParser :: Parser XmlValue
xmlNodeParser = do
  (tag, attributes) <- tagParser
  inner <- many xmlValueParser <|> pure []
  closingTagParser tag
  pure (XmlNode tag attributes inner)

xmlValueParser :: Parser XmlValue
xmlValueParser = xmlCommentParser <|> xmlNodeParser <|> xmlContentParser

xmlPrologParser :: Parser String
xmlPrologParser =
  wsParser
    *> stringParser "<?"
    *> spanParser (/= '>')
    <* charParser '>'
    <* wsParser

xmlHeaderParser :: Parser String
xmlHeaderParser = xmlPrologParser

xmlParser :: Parser XmlValue
xmlParser = do
  xmlHeaderParser <|> pure []
  xmlNodeParser