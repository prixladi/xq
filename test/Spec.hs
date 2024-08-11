module Main (main) where

import Helpers
import XmlParserTest
import XqParserTest
import XqRunnerTest

printResult :: (String, Either String ()) -> IO ()
printResult (name, Left err) = putStrLn $ "  " ++ name ++ " - Error: " ++ err
printResult (name, Right ()) = putStrLn $ "  " ++ name ++ " - Success"

main :: IO ()
main = do
  putStrLn "xmlParserTests:"
  mapM_ printResult $ runAll XmlParserTestModule

  putStrLn "xqParserTests:"
  mapM_ printResult $ runAll XqParserTestModule

  putStrLn "xqRunnerTests:"
  mapM_ printResult $ runAll XqRunnerTestModule