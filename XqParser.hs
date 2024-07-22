module XqParser where

import Control.Applicative
import Parser
import XmlParser

type IsRecursive = Bool

data XqNodeMatcher = WildcardNode | PreciseNode String deriving (Show, Eq)

data XqValue = XqNode IsRecursive XqNodeMatcher deriving (Show, Eq)

xqNodeParser :: Parser XqValue
xqNodeParser =
  XqNode
    <$> ((True <$ stringParser "//") <|> (False <$ stringParser "/"))
    <*> (PreciseNode <$> xmlNameParser <|> WildcardNode <$ charParser '*')

xqParser :: Parser [XqValue]
xqParser = many xqNodeParser