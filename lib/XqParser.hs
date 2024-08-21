{-# LANGUAGE ExistentialQuantification #-}

module XqParser
  ( Cmp (..),
    PositionSelector (..),
    TagSelector (..),
    AttributeSelector (..),
    ContentSelector (..),
    XqSelector (..),
    XqValue (..),
    xqParser,
  )
where

import Control.Applicative
import Parser
import XmlParser

type IsRecursive = Bool

data Cmp = Eq | NotEq | Gt | Lt deriving (Show, Eq)

data PositionSelector = PrecisePosition Cmp Int | LastPosition deriving (Show, Eq)

data TagSelector = WildcardTag | PreciseTag String deriving (Show, Eq)

data AttributeSelector = BasicAttribute String (Maybe String) deriving (Show, Eq)

data ContentSelector = StringContent Cmp String | NumberContent Cmp Int deriving (Show, Eq)

data XqSelector
  = XqTag TagSelector
  | XqPosition PositionSelector
  | XqAttribute AttributeSelector
  | XqContent ContentSelector
  | XqChild [XqValue]
  deriving (Show, Eq)

data XqValue = XqNode IsRecursive [XqSelector] deriving (Show, Eq)

xqParser :: Parser [XqValue]
xqParser = many nodeParser

nodeParser :: Parser XqValue
nodeParser =
  XqNode
    <$> ((True <$ stringParser "//") <|> (False <$ charParser '/'))
    <*> selectorsParser

selectorsParser :: Parser [XqSelector]
selectorsParser = notNull ((:) <$> tagSelector <*> many otherSelector)
  where
    tagSelector = XqTag <$> tagSelectorParser
    otherSelector = charParser '[' *> selectorParser <* charParser ']'

selectorParser :: Parser XqSelector
selectorParser =
  (XqPosition <$> positionSelectorParser)
    <|> (XqAttribute <$> attributeSelectorParser)
    <|> (XqContent <$> contentSelectorParser)
    <|> (XqChild <$> childSelectorParser)

tagSelectorParser :: Parser TagSelector
tagSelectorParser =
  (PreciseTag <$> xmlNameParser)
    <|> (WildcardTag <$ charParser '*')

-- for child selector we allow just the relative path hence first node
-- hence first node needs to be always present and first not will not be recursive
childSelectorParser :: Parser [XqValue]
childSelectorParser = ((:) . XqNode False <$> selectorsParser) <*> xqParser

contentSelectorParser :: Parser ContentSelector
contentSelectorParser =
  stringParser "text()"
    *> ( NumberContent <$> cmpParser <*> intParser
           <|> StringContent <$> cmpParser <*> xqStringLiteralParser
       )

attributeSelectorParser :: Parser AttributeSelector
attributeSelectorParser =
  BasicAttribute
    <$> (charParser '@' *> xmlNameParser)
    <*> optional (charParser '=' *> xqStringLiteralParser)

positionSelectorParser :: Parser PositionSelector
positionSelectorParser =
  (PrecisePosition Eq <$> intParser)
    <|> (PrecisePosition <$ stringParser "position()" <*> cmpParser <*> intParser)
    <|> (LastPosition <$ stringParser "last()")

cmpParser :: Parser Cmp
cmpParser =
  Eq <$ charParser '='
    <|> NotEq <$ stringParser "!="
    <|> Gt <$ stringParser ">"
    <|> Lt <$ stringParser "<"

xqStringLiteralParser :: Parser String
xqStringLiteralParser =
  charParser '\''
    *> spanParser (/= '\'')
    <* charParser '\''