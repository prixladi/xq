module Main (main) where

import Data.Monoid
import Parser
import Serialize
import System.Environment
import System.Exit
import Utils
import XmlParser
import XqParser
import XqRunner

type CliError = (Int, String)

main :: IO ()
main = getArgs >>= runApp >>= printAndExit

runApp :: [String] -> IO (Either CliError String)
runApp args | hasSwitch ["--help", "-h"] args = (pure . Right) help
runApp args | hasSwitch ["--version", "-v"] args = (pure . Right) version
runApp args
  | length args == 2 = run <$> readFile (args !! 1)
  | length args == 1 = run <$> getContents
  where
    xq = head args
    run xml = serialize <$> queryXml xq xml
runApp _ = (pure . Left) (123, "Invalid arguments")

queryXml :: String -> String -> Either CliError [XmlValue]
queryXml xqString xmlString =
  runXq <$> xq <*> xml
  where
    xq = maybeToEither (202, "Unable to parse the XQuery.") (runParser xqParser xqString >>= expectInputEmpty)
    xml = maybeToEither (201, "Unable to parse the XML.") (runParser xmlParser xmlString >>= expectInputEmpty)
    expectInputEmpty (input, res) = if null input then Just res else Nothing

printAndExit :: Either CliError String -> IO ()
printAndExit res = case res of
  Left (exitCode, err) -> do
    putStrLn err
    putStrLn shortHelp
    exitWith (ExitFailure exitCode)
  Right out -> do
    putStrLn out
    exitSuccess

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither s Nothing = Left s
maybeToEither _ (Just val) = Right val

hasSwitch :: [String] -> [String] -> Bool
hasSwitch switches = getAny . foldMap (Any . anyOf ((\x -> (== x)) <$> switches))

shortHelp :: String
shortHelp = "try 'xq --help' for more information"

help :: String
help =
  "Usage: xq [options] <xQuery> [xmlFilePath]\n\
  \  hint: if xmlFilePath is not provided xq will expect XML content on stdin\n\
  \Options:\n\
  \  -h --help       Prints help message and exits\n\
  \  -v --version       Prints version and exits\n\
  \Examples: xq \"//book/*[@lang]\" \"./bookstore.xml\"\n\
  \          cat \"./bookstore.xml\" | xq \"//book/*[@id='5']\""

version :: String
version = "v0.0.1"