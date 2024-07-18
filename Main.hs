module Main where

import Parser
import XMLParser

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
  input <- readFile fileName
  pure (snd <$> runParser parser input)

main :: IO ()
main = undefined