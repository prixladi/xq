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
  "Usage: xq <xQuery> <xmlFilePath>\n\
  \Usage: xq <xQuery> {xmlData from stdin}\n\
  \Example: xq \"//book/*\" \"./bookstore.xml\"\n\
  \Example: cat \"./bookstore.xml\" | Examples: xq \"//book/*\""

exit :: String -> IO ()
exit err = do
  putStrLn err
  putStrLn usage
  exitFailure

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither s Nothing = Left s
maybeToEither _ (Just val) = Right val

inputIsEmpty :: (String, a) -> Maybe (String, a)
inputIsEmpty res = if null (fst res) then Just res else Nothing

queryXml :: (String, String) -> Either String [XmlValue]
queryXml (x, q) = do
  (_, xml) <- maybeToEither "Unable to parse the XML." (runParser xmlParser x >>= inputIsEmpty)
  (_, xq) <- maybeToEither "Unable to parse the XQuery." (runParser xqParser q >>= inputIsEmpty)
  Right $ runXq xq xml

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
    Right value -> putStrLn (serialize value)
    Left err -> exit err