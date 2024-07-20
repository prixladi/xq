module XQueryExecutor where

import Control.Monad
import Debug.Trace
import XMLParser
import XQueryParser

filterMatchingNodes :: XQueryNodeMatcher -> XMLValue -> Bool
filterMatchingNodes WildcardNode (XMLNode {}) = True
filterMatchingNodes (PreciseNode en) (XMLNode n _ _) | en == n = True
filterMatchingNodes _ _ = False

getChildren :: XMLValue -> [XMLValue]
getChildren (XMLNode _ _ c) = c
getChildren _ = []

executeXQueryValue :: XQueryValue -> XMLValue -> [XMLValue]
executeXQueryValue (XQueryNode False en) xml = filter (filterMatchingNodes en) (getChildren xml)
executeXQueryValue (XQueryNode True en) xml =
  executeXQueryValue (XQueryNode False en) xml ++ (executeXQueryValue (XQueryNode True en) =<< getChildren xml)

executeXQueryValues :: XQueryValue -> [XMLValue] -> [XMLValue]
executeXQueryValues query xml = executeXQueryValue query =<< xml

executeXQuery :: [XQueryValue] -> XMLValue -> [XMLValue]
executeXQuery [] xml = [xml]
executeXQuery query xml = foldl (flip executeXQueryValues) [rootNode] query
  where
    rootNode = XMLNode "root" [] [xml]