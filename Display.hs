module Display where

import Data.List
import XMLParser
import XQueryParser

displayXMLValue :: XMLValue -> String
displayXMLValue (XMLContent s) = s
displayXMLValue (XMLNode tag attrs children) =
  "<" ++ tag ++ displayAttrs attrs ++ ">" ++ intercalate "" (displayXMLValue <$> children) ++ "</" ++ tag ++ ">"
  where
    displayAttrs [] = ""
    displayAttrs attrs = " " ++ unwords ((\(k, v) -> k ++ "=\"" ++ v ++ "\"") <$> attrs)

displayXMLValues :: [XMLValue] -> String
displayXMLValues values = intercalate "" (displayXMLValue <$> values)

displayXQueryNodeMatcher :: XQueryNodeMatcher -> String
displayXQueryNodeMatcher WildcardNode = "*"
displayXQueryNodeMatcher (PreciseNode p) = p

displayXQueryValue :: XQueryValue -> String
displayXQueryValue (XQueryNode isRec matcher) = path ++ displayXQueryNodeMatcher matcher
  where
    path = if isRec then "//" else ""

displayXQueryValues :: [XQueryValue] -> String
displayXQueryValues values = intercalate "" (displayXQueryValue <$> values)