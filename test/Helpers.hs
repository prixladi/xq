module Helpers(TestModule(..), expectEq) where

class TestModule g where
  runAll :: g -> [(String, Either String ())]

expectEq :: (Eq a, Show a) => a -> a -> Either String ()
expectEq a b = if a == b then Right () else Left ("Expected : " ++ show a ++ " Got: " ++ show b)
