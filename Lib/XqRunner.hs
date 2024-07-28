module Lib.XqRunner where

import Control.Monad
import Lib.Utils
import Lib.XmlParser
import Lib.XqParser

matchTag :: TagSelector -> XmlValue -> Bool
matchTag WildcardTag _ = True
matchTag (PreciseTag en) (XmlNode n _ _) = en == n
matchTag _ _ = False

matchPosition :: PositionSelector -> Int -> (Int, XmlValue) -> Bool
matchPosition (PrecisePosition Eq ep) _ (p, XmlNode {}) = p == ep
matchPosition (PrecisePosition Gt ep) _ (p, XmlNode {}) = p > ep
matchPosition (PrecisePosition Lt ep) _ (p, XmlNode {}) = p < ep
matchPosition LastPosition len (p, XmlNode {}) = p == len
matchPosition _ _ _ = False

matchAttribute :: AttributeSelector -> XmlValue -> Bool
matchAttribute (BasicAttribute name Nothing) (XmlNode _ attributes _) = name `elem` (fst <$> attributes)
matchAttribute (BasicAttribute name (Just value)) (XmlNode _ attributes _) = (name, value) `elem` attributes
matchAttribute _ _ = False

matchNode :: Selector -> Int -> (Int, XmlValue) -> Bool
matchNode (Tag tag) _ (_, node) = matchTag tag node
matchNode (Attribute att) _ (_, node) = matchAttribute att node
matchNode (Position pos) len node = matchPosition pos len node

filterNodes :: [Selector] -> [XmlValue] -> [XmlValue]
filterNodes match xml = foldl foldBySelector xml match
  where
    -- Then we need to assign number for every element in each iteration for positional matching
    foldBySelector x m = snd <$> filter (matchNode m (length x)) (numbered x)

getChildrenNodes :: XmlValue -> [XmlValue]
getChildrenNodes (XmlNode _ _ c) = filter isNode c
  where
    isNode (XmlNode {}) = True
    isNode _ = False
getChildrenNodes _ = []

runXqValue :: XqValue -> XmlValue -> [XmlValue]
runXqValue (XqNode False s) xml = filterNodes s (getChildrenNodes xml)
runXqValue (XqNode True s) xml = currentResult ++ childrenResult
  where
    currentResult = runXqValue (XqNode False s) xml
    childrenResult = getChildrenNodes xml >>= runXqValue (XqNode True s)

runXq :: [XqValue] -> XmlValue -> [XmlValue]
runXq [] xml = [xml]
runXq query xml = foldl foldValue [XmlNode "root" [] [xml]] query
  where
    foldValue acc cur = acc >>= runXqValue cur