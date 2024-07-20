module Display where

import Data.List
import XMLParser
import XQParser

class Display g where
  display :: g -> String

instance Display XMLValue where
  display :: XMLValue -> String
  display (XMLContent s) = s
  display (XMLNode tag attrs children) =
    "<" ++ tag ++ displayAttrs attrs ++ ">" ++ intercalate "" (display <$> children) ++ "</" ++ tag ++ ">"
    where
      displayAttrs [] = ""
      displayAttrs attrs = " " ++ unwords ((\(k, v) -> k ++ "=\"" ++ v ++ "\"") <$> attrs)

instance Display [XMLValue] where
  display :: [XMLValue] -> String
  display values = intercalate "\n" (display <$> values)

instance Display XQNodeMatcher where
  display :: XQNodeMatcher -> String
  display WildcardNode = "*"
  display (PreciseNode p) = p

instance Display XQValue where
  display :: XQValue -> String
  display (XQueryNode isRec matcher) = (if isRec then "//" else "") ++ display matcher

instance Display [XQValue] where
  display :: [XQValue] -> String
  display values = intercalate "" (display <$> values)