module Main where

import Debug.Trace
import Parser
import XMLParser
import XQueryExecutor
import XQueryParser

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
  input <- readFile fileName
  pure (snd <$> runParser parser input)

queryXML :: String -> String -> Maybe [XMLValue]
queryXML q x = do
  (_, query) <- runParser xQueryParser q
  (_, xml) <- runParser xmlParser x

  pure $ executeXQuery query xml

queryFile :: String -> FilePath -> IO (Maybe [XMLValue])
queryFile query fileName = do
  fileContent <- readFile fileName
  pure (queryXML query fileContent)

main :: IO ()
main = undefined