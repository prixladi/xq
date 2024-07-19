module XQueryParser where

import Control.Applicative
import Parser
import XMLParser

type IsRelative = Bool

data XQueryValue = XQueryNode IsRelative String deriving (Show)

xQueryNodeParser :: Parser XQueryValue
xQueryNodeParser = do
  pathType <- stringParser "//" <|> stringParser "/"
  XQueryNode (pathType == "//") <$> xmlNameParser

xQueryParser :: Parser [XQueryValue]
xQueryParser = many xQueryNodeParser