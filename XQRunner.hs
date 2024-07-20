module XQRunner where

import Control.Monad
import Debug.Trace
import XMLParser
import XQParser

matchNode :: XQNodeMatcher -> XMLValue -> Bool
matchNode WildcardNode (XMLNode {}) = True
matchNode (PreciseNode en) (XMLNode n _ _) | en == n = True
matchNode _ _ = False

getChildren :: XMLValue -> [XMLValue]
getChildren (XMLNode _ _ c) = c
getChildren _ = []

runXQValue :: XQValue -> XMLValue -> [XMLValue]
runXQValue (XQueryNode False en) xml = filter (matchNode en) (getChildren xml)
runXQValue (XQueryNode True en) xml =
  runXQValue (XQueryNode False en) xml ++ (runXQValue (XQueryNode True en) =<< getChildren xml)

runXQValues :: XQValue -> [XMLValue] -> [XMLValue]
runXQValues query xml = runXQValue query =<< xml

runXQuery :: [XQValue] -> XMLValue -> [XMLValue]
runXQuery [] xml = [xml]
runXQuery query xml = foldl (flip runXQValues) [rootNode] query
  where
    rootNode = XMLNode "root" [] [xml]