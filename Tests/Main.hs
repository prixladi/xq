module Tests.Main where

import Tests.Helpers
import Tests.XMLParser
import Tests.XQParser

printResult :: (String, Either String ()) -> IO ()
printResult (name, Left error) = putStrLn $ "  " ++ name ++ " - Error: " ++ error
printResult (name, Right ()) = putStrLn $ "  " ++ name ++ " - Success"

main :: IO ()
main = do
  putStrLn "xmlParserTests:"
  mapM_ printResult $ runAll XMLParserTestModule

  putStrLn "xqParserTests:"
  mapM_ printResult $ runAll XQParserTestModule