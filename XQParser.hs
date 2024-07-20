module XQParser where

import Control.Applicative
import Parser
import XMLParser

type IsRecursive = Bool

data XQNodeMatcher = WildcardNode | PreciseNode String deriving (Show)

data XQValue = XQueryNode IsRecursive XQNodeMatcher deriving (Show)

xQNodeParser :: Parser XQValue
xQNodeParser =
  XQueryNode
    <$> ((True <$ stringParser "//") <|> (False <$ stringParser "/"))
    <*> (PreciseNode <$> xmlNameParser <|> WildcardNode <$ charParser '*')

xQParser :: Parser [XQValue]
xQParser = many xQNodeParser