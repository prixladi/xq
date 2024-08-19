module XqRunner (runXq) where

import Utils
import XmlParser
import XqParser

runXq :: [XqValue] -> XmlValue -> [XmlValue]
runXq [] xml = [xml]
runXq query xml = foldl foldValue [XmlNode "root" [] [xml]] query
  where
    foldValue acc cur = acc >>= runXqValue cur

runXqValue :: XqValue -> XmlValue -> [XmlValue]
runXqValue (XqNode False s) xml = filterNodes s (getChildrenNodes xml)
runXqValue xq@(XqNode True s) xml = currentResult ++ childrenResult
  where
    currentResult = runXqValue (XqNode False s) xml
    childrenResult = getChildrenNodes xml >>= runXqValue xq

getChildrenNodes :: XmlValue -> [XmlValue]
getChildrenNodes (XmlNode _ _ c) = filter isNode c
  where
    isNode (XmlNode {}) = True
    isNode _ = False
getChildrenNodes _ = []

filterNodes :: [Selector] -> [XmlValue] -> [XmlValue]
filterNodes selectors xml = foldl foldBySelector xml selectors
  where
    -- We need to assign number for every element in each iteration for positional matching
    foldBySelector x s = snd <$> filter (matchNode s (length x)) (numbered x)

matchNode :: Selector -> Int -> (Int, XmlValue) -> Bool
matchNode (Tag tag) _ (_, node) = matchTag tag node
matchNode (Attribute att) _ (_, node) = matchAttribute att node
matchNode (Position pos) len node = matchPosition pos len node

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
