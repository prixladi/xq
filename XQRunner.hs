module XQRunner where

import Control.Monad
import Debug.Trace
import XmlParser
import XQParser

matchNode :: XQNodeMatcher -> XmlValue -> Bool
matchNode WildcardNode (XmlNode {}) = True
matchNode (PreciseNode en) (XmlNode n _ _) | en == n = True
matchNode _ _ = False

getChildren :: XmlValue -> [XmlValue]
getChildren (XmlNode _ _ c) = c
getChildren _ = []

runXQValue :: XQValue -> XmlValue -> [XmlValue]
runXQValue (XQNode False en) xml = filter (matchNode en) (getChildren xml)
runXQValue (XQNode True en) xml =
  runXQValue (XQNode False en) xml ++ (runXQValue (XQNode True en) =<< getChildren xml)

runXQValues :: XQValue -> [XmlValue] -> [XmlValue]
runXQValues query xml = runXQValue query =<< xml

runXQ :: [XQValue] -> XmlValue -> [XmlValue]
runXQ [] xml = [xml]
runXQ query xml = foldl (flip runXQValues) [rootNode] query
  where
    rootNode = XmlNode "root" [] [xml]