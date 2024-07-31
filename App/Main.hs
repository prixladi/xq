module App.Main where

import Control.Monad
import Lib.Parser
import Lib.Serialize
import Lib.XmlParser
import Lib.XqParser
import Lib.XqRunner
import System.Directory.Internal.Prelude
import System.Exit

usage :: String
usage =
  "Usage:    xq <xQuery> <xmlFilePath>\n\
  \          xq <xQuery> {xmlStdin}\n\
  \Example:  xq \"//book/*[@lang]\" \"./bookstore.xml\"\n\
  \          cat \"./bookstore.xml\" | xq \"//book/*[@id='5']\""

exit :: String -> IO ()
exit err = do
  putStrLn err
  putStrLn usage
  exitFailure

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither s Nothing = Left s
maybeToEither _ (Just val) = Right val

expectInputEmpty :: (String, a) -> Maybe a
expectInputEmpty (input, res) | null input = Just res
expectInputEmpty _ = Nothing

queryXml :: (String, String) -> Either String [XmlValue]
queryXml (x, q) =
  runXq <$> xq <*> xml
  where
    xml = maybeToEither "Unable to parse the XML." (runParser xmlParser x >>= expectInputEmpty)
    xq = maybeToEither "Unable to parse the XQuery." (runParser xqParser q >>= expectInputEmpty)

getInputs :: IO (Either String (String, String))
getInputs = do
  args <- getArgs
  let argLen = length args

  if argLen /= 2 && argLen /= 1
    then pure $ Left "Invalid number of arguments"
    else do
      xmlString <-
        if argLen == 2
          then readFile (args !! 1)
          else getContents
      pure $ Right (xmlString, head args)

main :: IO ()
main = do
  inputs <- getInputs
  case inputs >>= queryXml of
    Right value -> putStr $ serialize value
    Left err -> exit err