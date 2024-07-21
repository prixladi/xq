module Tests.Main where

import Tests.Helpers
import Tests.XmlParser
import Tests.XQParser

printResult :: (String, Either String ()) -> IO ()
printResult (name, Left error) = putStrLn $ "  " ++ name ++ " - Error: " ++ error
printResult (name, Right ()) = putStrLn $ "  " ++ name ++ " - Success"

main :: IO ()
main = do
  putStrLn "xmlParserTests:"
  mapM_ printResult $ runAll XmlParserTestModule

  putStrLn "xqParserTests:"
  mapM_ printResult $ runAll XQParserTestModule