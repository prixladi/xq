module Test.Main where

import Test.Helpers
import Test.XmlParser
import Test.XqParser
import Test.XqRunner

printResult :: (String, Either String ()) -> IO ()
printResult (name, Left error) = putStrLn $ "  " ++ name ++ " - Error: " ++ error
printResult (name, Right ()) = putStrLn $ "  " ++ name ++ " - Success"

main :: IO ()
main = do
  putStrLn "xmlParserTests:"
  mapM_ printResult $ runAll XmlParserTestModule

  putStrLn "xqParserTests:"
  mapM_ printResult $ runAll XqParserTestModule

  putStrLn "xqRunnerTests:"
  mapM_ printResult $ runAll XqRunnerTestModule