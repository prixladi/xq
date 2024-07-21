module XQParser where

import Control.Applicative
import Parser
import XmlParser

type IsRecursive = Bool

data XQNodeMatcher = WildcardNode | PreciseNode String deriving (Show, Eq)

data XQValue = XQNode IsRecursive XQNodeMatcher deriving (Show, Eq)

xQNodeParser :: Parser XQValue
xQNodeParser =
  XQNode
    <$> ((True <$ stringParser "//") <|> (False <$ stringParser "/"))
    <*> (PreciseNode <$> xmlNameParser <|> WildcardNode <$ charParser '*')

xQParser :: Parser [XQValue]
xQParser = many xQNodeParser