module XQueryExecutor where

import Control.Monad
import XMLParser
import XQueryParser

filterMatchingNodes :: String -> XMLValue -> Bool
filterMatchingNodes en (XMLNode n _ _) | en == n = True
filterMatchingNodes _ _ = False

getChildren :: XMLValue -> [XMLValue]
getChildren (XMLNode _ _ c) = c
getChildren _ = []

-- TODO PROBLEM
executeXQueryValue :: XQueryValue -> XMLValue -> [XMLValue]
executeXQueryValue (XQueryNode False en) xml = filter (filterMatchingNodes en) (getChildren xml)
executeXQueryValue (XQueryNode True en) xml = do
  matching <- executeXQueryValue (XQueryNode True en) <$> getChildren  xml
  executeXQueryValue (XQueryNode False en) xml ++ matching

executeXQueryValues :: XQueryValue -> [XMLValue] -> [XMLValue]
executeXQueryValues query xml = executeXQueryValue query =<< xml

executeXQueryI :: [XQueryValue] -> [XMLValue] -> [XMLValue]
executeXQueryI xs xml = foldl (flip executeXQueryValues) xml xs

executeXQuery :: [XQueryValue] -> XMLValue -> [XMLValue]
executeXQuery query xml = executeXQueryI query [XMLNode "root" [] [xml]]