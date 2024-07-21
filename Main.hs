module Main where

import Display
import Parser
import XmlParser
import XQParser
import XQRunner

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
  input <- readFile fileName
  pure (snd <$> runParser parser input)

queryXml :: String -> String -> Maybe [XmlValue]
queryXml q x = do
  (_, query) <- runParser xQParser q
  (_, xml) <- runParser xmlParser x

  pure $ runXQ query xml

queryFile :: String -> FilePath -> IO (Maybe [XmlValue])
queryFile query fileName = do
  fileContent <- readFile fileName
  pure (queryXml query fileContent)

main :: IO ()
main = do
  res <- queryFile "/bookstore/*//book" "./output/basic.xml"
  case display <$> res of
    Just a -> putStrLn a
    Nothing -> error "Err."
  pure ()