module XPathParser where

import Control.Applicative
import Parser
import XMLParser

data XPathValue = XPathNode Bool String | XPathWildcardNode Bool deriving (Show)

parseXPathNode :: Parser XPathValue
parseXPathNode = do
  path <- stringParser "//" <|> stringParser "/"
  XPathNode (path == "//") <$> xmlNameParser

parseXPath :: Parser [XPathValue]
parseXPath = many parseXPathNode