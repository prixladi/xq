module XQueryParser where

import Control.Applicative
import Parser
import XMLParser

type IsRelative = Bool

data XQueryValue = XQueryNode IsRelative String deriving (Show)

parseXQueryNode :: Parser XQueryValue
parseXQueryNode = do
  pathType <- stringParser "//" <|> stringParser "/"
  XQueryNode (pathType == "//") <$> xmlNameParser

parseXQuery :: Parser [XQueryValue]
parseXQuery = many parseXQueryNode