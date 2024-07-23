module Lib.Utils where
  
import Data.Char

spanList :: ([a] -> Bool) -> [a] -> ([a], [a])
spanList _ [] = ([], [])
spanList func list@(x : xs) =
  if func list
    then (x : ys, zs)
    else ([], list)
  where
    (ys, zs) = spanList func xs


trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace