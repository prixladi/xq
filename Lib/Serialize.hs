module Lib.Serialize where

import Data.List
import Lib.XmlParser
import Lib.XqParser

class Serialize g where
  serialize :: g -> String

instance Serialize XmlValue where
  serialize :: XmlValue -> String
  serialize (XmlContent s) = s
  serialize (XmlNode tag attrs children) =
    "<" ++ tag ++ serializeAttrs attrs ++ ">" ++ intercalate "" (serialize <$> children) ++ "</" ++ tag ++ ">"
    where
      serializeAttrs [] = ""
      serializeAttrs attrs = " " ++ unwords ((\(k, v) -> k ++ "=\"" ++ v ++ "\"") <$> attrs)

instance Serialize [XmlValue] where
  serialize :: [XmlValue] -> String
  serialize values = intercalate "\n" (serialize <$> values)

instance Serialize XqNodeMatcher where
  serialize :: XqNodeMatcher -> String
  serialize WildcardNode = "*"
  serialize (PreciseNode p) = p

instance Serialize XqValue where
  serialize :: XqValue -> String
  serialize (XqNode isRec matcher) = (if isRec then "//" else "") ++ serialize matcher

instance Serialize [XqValue] where
  serialize :: [XqValue] -> String
  serialize values = intercalate "" (serialize <$> values)