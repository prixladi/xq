module Lib.XqParser where

import Control.Applicative
import Lib.Parser
import Lib.XmlParser

type IsRecursive = Bool

data Cmp = Eq | Gt | Lt deriving (Show, Eq)

data PositionSelector = PrecisePosition Cmp Int | LastPosition deriving (Show, Eq)

data TagSelector = WildcardTag | PreciseTag String deriving (Show, Eq)

data Selector = Position PositionSelector | Tag TagSelector | Attribute (String, Maybe String) deriving (Show, Eq)

data XqValue = XqNode IsRecursive [Selector] deriving (Show, Eq)

positionSelectorParser :: Parser PositionSelector
positionSelectorParser =
  (PrecisePosition Eq <$> intParser)
    <|> (PrecisePosition Eq <$> (stringParser "position()=" *> intParser))
    <|> (PrecisePosition Lt <$> (stringParser "position()<" *> intParser))
    <|> (PrecisePosition Gt <$> (stringParser "position()>" *> intParser))
    <|> (LastPosition <$ stringParser "last()")

attributeSelectorParser :: Parser (String, Maybe String)
attributeSelectorParser =
  (,)
    <$> spanParser (/= '=')
    <*> optional (charParser '=' *> stringLiteralParser)

selectorsParser :: Parser [Selector]
selectorsParser = notNull ((++) <$> tagSelector <*> many otherSelector)
  where
    -- Just one tag selector is allowed and it must be first, it is also optional
    tagSelector = (pure . Tag <$> (PreciseTag <$> xmlNameParser <|> WildcardTag <$ charParser '*')) <|> pure []
    otherSelector = charParser '[' *> ((Position <$> positionSelectorParser) <|> (Attribute <$> attributeSelectorParser)) <* charParser ']'

nodeParser :: Parser XqValue
nodeParser =
  XqNode
    <$> ((True <$ stringParser "//") <|> (False <$ charParser '/'))
    <*> selectorsParser

xqParser :: Parser [XqValue]
xqParser = many nodeParser
