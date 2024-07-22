module Lib.XqRunner where

import Control.Monad
import Lib.XmlParser
import Lib.XqParser

matchNode :: XqNodeMatcher -> XmlValue -> Bool
matchNode WildcardNode (XmlNode {}) = True
matchNode (PreciseNode en) (XmlNode n _ _) | en == n = True
matchNode _ _ = False

getChildren :: XmlValue -> [XmlValue]
getChildren (XmlNode _ _ c) = c
getChildren _ = []

runXqValue :: XqValue -> XmlValue -> [XmlValue]
runXqValue (XqNode False en) xml = filter (matchNode en) (getChildren xml)
runXqValue (XqNode True en) xml =
  runXqValue (XqNode False en) xml ++ (runXqValue (XqNode True en) =<< getChildren xml)

runXqValues :: XqValue -> [XmlValue] -> [XmlValue]
runXqValues query xml = runXqValue query =<< xml

runXq :: [XqValue] -> XmlValue -> [XmlValue]
runXq [] xml = [xml]
runXq query xml = foldl (flip runXqValues) [rootNode] query
  where
    rootNode = XmlNode "root" [] [xml]