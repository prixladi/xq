module Display where

import Data.List
import XMLParser
import XQParser

displayXMLValue :: XMLValue -> String
displayXMLValue (XMLContent s) = s
displayXMLValue (XMLNode tag attrs children) =
  "<" ++ tag ++ displayAttrs attrs ++ ">" ++ intercalate "" (displayXMLValue <$> children) ++ "</" ++ tag ++ ">"
  where
    displayAttrs [] = ""
    displayAttrs attrs = " " ++ unwords ((\(k, v) -> k ++ "=\"" ++ v ++ "\"") <$> attrs)

displayXMLValues :: [XMLValue] -> String
displayXMLValues values = intercalate "" (displayXMLValue <$> values)

displayXQueryNodeMatcher :: XQNodeMatcher -> String
displayXQueryNodeMatcher WildcardNode = "*"
displayXQueryNodeMatcher (PreciseNode p) = p

displayXQueryValue :: XQValue -> String
displayXQueryValue (XQueryNode isRec matcher) = path ++ displayXQueryNodeMatcher matcher
  where
    path = if isRec then "//" else ""

displayXQueryValues :: [XQValue] -> String
displayXQueryValues values = intercalate "" (displayXQueryValue <$> values)