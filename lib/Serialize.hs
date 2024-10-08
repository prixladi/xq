{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Serialize (Serialize (..)) where

import Data.List (intercalate)
import XmlParser

class Serialize g where
  serialize :: g -> String

instance Serialize [XmlValue] where
  serialize :: [XmlValue] -> String
  serialize values = intercalate "\n" (serialize <$> values)

instance Serialize XmlValue where
  serialize :: XmlValue -> String
  serialize (XmlContent s) = s
  serialize (XmlComment s) = "<!--" ++ s ++ "-->"
  serialize (XmlProcessingInstruction s) = "<?" ++ s ++ ">"
  serialize (XmlNode tag attrs children) =
    openingTag ++ intercalate "" (serialize <$> children) ++ closingTag
    where
      attributes = if null attrs then "" else " " ++ unwords ((\(k, v) -> k ++ "=\"" ++ v ++ "\"") <$> attrs)
      openingTag = "<" ++ tag ++ attributes ++ ">"
      closingTag = "</" ++ tag ++ ">"
