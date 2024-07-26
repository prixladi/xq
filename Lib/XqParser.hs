module Lib.XqParser where

import Control.Applicative
import Lib.Parser
import Lib.XmlParser

type IsRecursive = Bool

data Cmp = Eq | Gt | Lt deriving (Show, Eq)

data PositionSelector = PrecisePosition Cmp Int | LastPosition deriving (Show, Eq)

data TagSelector = WildcardTag | PreciseTag String deriving (Show, Eq)

data Selector = Position PositionSelector | Tag TagSelector deriving (Show, Eq)

data XqValue = XqNode IsRecursive [Selector] deriving (Show, Eq)

xqMatchPosParser :: Parser PositionSelector
xqMatchPosParser =
  (PrecisePosition Eq <$> intParser)
    <|> (PrecisePosition Eq <$> (stringParser "position()=" *> intParser))
    <|> (PrecisePosition Lt <$> (stringParser "position()<" *> intParser))
    <|> (PrecisePosition Gt <$> (stringParser "position()>" *> intParser))
    <|> (LastPosition <$ stringParser "last()")

xqMatchParser :: Parser Selector
xqMatchParser =
  charParser '['
    *> (Position <$> xqMatchPosParser)
    <* charParser ']'

xqNodeParser :: Parser XqValue
xqNodeParser =
  XqNode
    <$> ((True <$ stringParser "//") <|> (False <$ charParser '/'))
    <*> notNull ((++) <$> tagSelector <*> otherSelectors)
  where
    -- Just one tag selector is allowed and it must be first, it is also optional
    tagSelector = pure . Tag <$> (PreciseTag <$> xmlNameParser <|> WildcardTag <$ charParser '*') <|> pure []
    otherSelectors = many xqMatchParser <|> pure []

xqParser :: Parser [XqValue]
xqParser = many xqNodeParser