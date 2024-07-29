module Lib.XmlParser where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Functor
import Data.List
import Lib.Parser
import Lib.Utils

type Attribute = (String, String)

data XmlValue
  = XmlComment String
  | XmlProcessingInstruction String
  | XmlContent String
  | XmlNode String [Attribute] [XmlValue]
  deriving (Show, Eq)

xmlNameParser :: Parser String
xmlNameParser =
  (:)
    <$> charPredicateParser (anyOf [isLetter, (== '_')])
    <*> spanParser (anyOf [isLetter, isNumber, (== '-'), (== '_'), (== '.')])

attributeParser :: Parser Attribute
attributeParser =
  (,)
    <$ wsParser
    <*> xmlNameParser
    <* charWsParser '='
    <*> stringLiteralParser

tagParser :: Parser (String, [Attribute])
tagParser =
  (,)
    <$ wsParser
    <* charParser '<'
    <*> xmlNameParser
    <*> many attributeParser
    <* charWsParser '>'

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

xmlProcessingInstructionsParser :: Parser XmlValue
xmlProcessingInstructionsParser =
  XmlProcessingInstruction
    <$> ( wsParser
            *> stringParser "<?"
            *> (unwords <$> many contentParts)
            <* charParser '>'
            <* wsParser
        )
  where
    contentParts = notNull (spanParser $ noneOf [(== '>'), (== '"')]) <|> stringLiteralParser

xmlNodeParser :: Parser XmlValue
xmlNodeParser = do
  (tag, attributes) <- tagParser
  inner <- many xmlValueParser
  closingTagParser tag
  pure (XmlNode tag attributes inner)

xmlValueParser :: Parser XmlValue
xmlValueParser =
  xmlCommentParser
    <|> xmlProcessingInstructionsParser
    <|> xmlNodeParser
    <|> xmlContentParser

-- | Combinator that wraps a parser with parsers that ignore surrounding processing instructions
ignorePi :: Parser a -> Parser a
ignorePi p = many xmlProcessingInstructionsParser *> p <* many xmlProcessingInstructionsParser

xmlParser :: Parser XmlValue
-- We start with 'xmlNodeParser' and not 'xmlValueParser' because there always need to be one root node
xmlParser = ignorePi xmlNodeParser