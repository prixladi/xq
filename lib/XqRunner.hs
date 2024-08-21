module XqRunner (runXq) where

import Data.Monoid
import Text.Read
import Utils
import XmlParser
import XqParser

runXq :: [XqValue] -> XmlValue -> [XmlValue]
runXq [] xml = [xml]
runXq query xml = runXq' query [XmlNode "root" [] [xml]]

runXq' :: [XqValue] -> [XmlValue] -> [XmlValue]
runXq' query xml = foldl foldValue xml query
  where
    foldValue acc cur = acc >>= runXqValue cur

runXqValue :: XqValue -> XmlValue -> [XmlValue]
runXqValue (XqNode False selectors) xml = filterNodes selectors (getChildrenNodes xml)
runXqValue xq@(XqNode True selectors) xml = currentResult ++ childrenResult
  where
    currentResult = runXqValue (XqNode False selectors) xml
    childrenResult = getChildrenNodes xml >>= runXqValue xq

getChildrenNodes :: XmlValue -> [XmlValue]
getChildrenNodes (XmlNode _ _ c) = [x | x@(XmlNode {}) <- c]
getChildrenNodes _ = []

filterNodes :: [XqSelector] -> [XmlValue] -> [XmlValue]
filterNodes selectors xml = foldl foldBySelector xml selectors
  where
    -- We need to assign number for every element in each iteration for positional matching
    foldBySelector x s = snd <$> filter (matchNode s (length x)) (numbered x)

matchNode :: XqSelector -> Int -> (Int, XmlValue) -> Bool
matchNode (XqTag tag) _ (_, node) = matchTag tag node
matchNode (XqAttribute att) _ (_, node) = matchAttribute att node
matchNode (XqContent content) _ (_, node) = matchContent content node
matchNode (XqPosition pos) len node = matchPosition pos len node
matchNode (XqChild xq) _ (_, node) = (not . null . runXq' xq) [node]

matchTag :: TagSelector -> XmlValue -> Bool
matchTag WildcardTag _ = True
matchTag (PreciseTag en) (XmlNode n _ _) = en == n
matchTag xq xml = error $ "Unable to match tag with provided arguments, xq: " ++ show xq ++ ", xml" ++ show xml

matchPosition :: PositionSelector -> Int -> (Int, XmlValue) -> Bool
matchPosition (PrecisePosition cmp ep) _ (p, XmlNode {}) = runCmp cmp ep p
matchPosition LastPosition len (p, XmlNode {}) = p == len
matchPosition xq _ xml = error $ "Unable to match position with provided arguments, xq: " ++ show xq ++ ", xml" ++ show xml

matchAttribute :: AttributeSelector -> XmlValue -> Bool
matchAttribute (BasicAttribute name Nothing) (XmlNode _ attributes _) = name `elem` (fst <$> attributes)
matchAttribute (BasicAttribute name (Just value)) (XmlNode _ attributes _) = (name, value) `elem` attributes
matchAttribute xq xml = error $ "Unable to match attribute with provided arguments, xq: " ++ show xq ++ ", xml" ++ show xml

matchContent :: ContentSelector -> XmlValue -> Bool
matchContent (StringContent cmp string) (XmlNode _ _ children) = anyContentMatches cmp string Just children
matchContent (NumberContent cmp number) (XmlNode _ _ children) = anyContentMatches cmp number readMaybe children
matchContent xq xml = error $ "Unable to match content with provided arguments, xq: " ++ show xq ++ ", xml" ++ show xml

anyContentMatches :: (Ord a) => Cmp -> a -> (String -> Maybe a) -> [XmlValue] -> Bool
anyContentMatches cmp expected tryParse = getAny . foldMap toAny
  where
    toAny = Any . f tryParse (runCmp cmp expected)
    f parse predicate (XmlContent content) | Just c <- parse content = predicate c
    f _ _ _ = False

runCmp :: (Ord a) => Cmp -> a -> a -> Bool
runCmp Eq a b = a == b
runCmp NotEq a b = a /= b
runCmp Gt a b = a < b
runCmp Lt a b = a > b
