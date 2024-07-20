module XQueryParser where

import Control.Applicative
import Parser
import XMLParser

type IsRecursive = Bool

data NodeMatcher = WildcardNode | PreciseNode String deriving (Show)

data XQueryValue = XQueryNode IsRecursive NodeMatcher deriving (Show)

xQueryNodeParser :: Parser XQueryValue
xQueryNodeParser =
  XQueryNode
    <$> ((True <$ stringParser "//") <|> (False <$ stringParser "/"))
    <*> (PreciseNode <$> xmlNameParser <|> WildcardNode <$ charParser '*')

xQueryParser :: Parser [XQueryValue]
xQueryParser = many xQueryNodeParser