module XQExecutor where

import Control.Monad
import Debug.Trace
import XMLParser
import XQParser

matchingNodes :: XQNodeMatcher -> XMLValue -> Bool
matchingNodes WildcardNode (XMLNode {}) = True
matchingNodes (PreciseNode en) (XMLNode n _ _) | en == n = True
matchingNodes _ _ = False

getChildren :: XMLValue -> [XMLValue]
getChildren (XMLNode _ _ c) = c
getChildren _ = []

executeXQValue :: XQValue -> XMLValue -> [XMLValue]
executeXQValue (XQueryNode False en) xml = filter (matchingNodes en) (getChildren xml)
executeXQValue (XQueryNode True en) xml =
  executeXQValue (XQueryNode False en) xml ++ (executeXQValue (XQueryNode True en) =<< getChildren xml)

executeXQValues :: XQValue -> [XMLValue] -> [XMLValue]
executeXQValues query xml = executeXQValue query =<< xml

executeXQuery :: [XQValue] -> XMLValue -> [XMLValue]
executeXQuery [] xml = [xml]
executeXQuery query xml = foldl (flip executeXQValues) [rootNode] query
  where
    rootNode = XMLNode "root" [] [xml]