module Utils (spanList, trim, numbered, allOf, anyOf, noneOf) where

import Data.Char

-- | Works similar to the 'GHC.List.span' but instead of running predicate just one current element runs predicate for the whole remaining string
--
-- >>> spanList (/= "end") "start other end"
-- ("start other ","end")
spanList :: ([a] -> Bool) -> [a] -> ([a], [a])
spanList _ [] = ([], [])
spanList func list@(x : xs) =
  if func list
    then (x : ys, zs)
    else ([], list)
  where
    (ys, zs) = spanList func xs

-- | Removes whitespace from the beginning and the end of the string
--
-- >>> trim "   test test     test         "
-- "test test     test"
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

-- | Returns copy of the list with tuples of elements and their order in the list starting with number 1
--
-- >>> numbered ['a', 'b', 'c']
-- [(1,'a'),(2,'b'),(3,'c')]
numbered :: [a] -> [(Int, a)]
numbered = f 1
  where
    f _ [] = []
    f pos (x : xs) = (pos, x) : f (pos + 1) xs

-- | Combines list of predicate functions that 'a' and returns predicate function that resolves if all of the input predicates are true
allOf :: [a -> Bool] -> a -> Bool
allOf predicate a = all (\p -> p a) predicate

-- | Combines list of predicate functions that 'a' and returns predicate function that resolves if any of the input predicates are true
anyOf :: [a -> Bool] -> a -> Bool
anyOf predicate a = any (\p -> p a) predicate

-- | Combines list of predicate functions that 'a' and returns predicate function that resolves if none of the input predicates are true
noneOf :: [a -> Bool] -> a -> Bool
noneOf predicate a = all (\p -> (not . p) a) predicate
