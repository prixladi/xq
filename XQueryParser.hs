module XQueryParser where

import Control.Applicative
import Parser
import XMLParser

type IsRecursive = Bool

data XQueryNodeMatcher = WildcardNode | PreciseNode String deriving (Show)

data XQueryValue = XQueryNode IsRecursive XQueryNodeMatcher deriving (Show)

xQueryNodeParser :: Parser XQueryValue
xQueryNodeParser =
  XQueryNode
    <$> ((True <$ stringParser "//") <|> (False <$ stringParser "/"))
    <*> (PreciseNode <$> xmlNameParser <|> WildcardNode <$ charParser '*')

xQueryParser :: Parser [XQueryValue]
xQueryParser = many xQueryNodeParser