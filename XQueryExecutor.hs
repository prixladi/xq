module XPathExecutor where

import Control.Monad
import XMLParser
import XQueryParser

filterMatchingNodes :: String -> XMLValue -> Bool
filterMatchingNodes en (XMLNode n _ _) | en == n = True
filterMatchingNodes _ _ = False

isXQueryNode :: XMLValue -> Bool
isXQueryNode (XMLNode {}) = True
isXQueryNode _ = False

getChildren :: XMLValue -> [XMLValue]
getChildren (XMLNode _ _ c) = c
getChildren _ = []

executeXQueryValue :: XQueryValue -> XMLValue -> [XMLValue]
executeXQueryValue (XQueryNode False en) xml = filter (filterMatchingNodes en) (getChildren xml)
executeXQueryValue (XQueryNode True en) xml = do
  matching <- executeXQueryValue (XQueryNode True en) <$> getChildren xml
  executeXQueryValue (XQueryNode False en) xml ++ matching

executeXQueryValues :: XQueryValue -> [XMLValue] -> [XMLValue]
executeXQueryValues value xmlValues = undefined

executeXQuery :: [XQueryValue] -> XMLValue -> [XMLValue]
executeXQuery a b = undefined