module Lib.Serialize where

import Data.List
import Lib.XmlParser

class Serialize g where
  serialize :: g -> String

instance Serialize XmlValue where
  serialize :: XmlValue -> String
  serialize (XmlContent s) = s
  serialize (XmlComment s) = "<!--" ++ s ++ "-->"
  serialize (XmlNode tag attrs children) =
    openingTag ++ intercalate "" (serialize <$> children) ++ closingTag
    where
      attributes = if null attrs then "" else " " ++ unwords ((\(k, v) -> k ++ "=\"" ++ v ++ "\"") <$> attrs)
      openingTag = "<" ++ tag ++ attributes ++ ">"
      closingTag = "</" ++ tag ++ ">"

instance Serialize [XmlValue] where
  serialize :: [XmlValue] -> String
  serialize values = intercalate "\n" (serialize <$> values)
