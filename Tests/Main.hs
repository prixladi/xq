module Tests.Main where

import Tests.Helpers
import Tests.XmlParser
import Tests.XqParser
import Tests.XqRunner

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