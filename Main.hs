module Main where

import Serialize
import Parser
import XmlParser
import XqParser
import XqRunner

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
  input <- readFile fileName
  pure (snd <$> runParser parser input)

queryXml :: String -> String -> Maybe [XmlValue]
queryXml q x = do
  (_, query) <- runParser xqParser q
  (_, xml) <- runParser xmlParser x

  pure $ runXq query xml

queryFile :: String -> FilePath -> IO (Maybe [XmlValue])
queryFile query fileName = do
  fileContent <- readFile fileName
  pure (queryXml query fileContent)

main :: IO ()
main = do
  res <- queryFile "/bookstore/*//book" "./output/basic.xml"
  case serialize <$> res of
    Just a -> putStrLn a
    Nothing -> error "Err."
  pure ()