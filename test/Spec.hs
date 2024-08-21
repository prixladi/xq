module Main (main) where

import Data.Either
import Helpers
import System.Exit
import XmlParserTest
import XqParserTest
import XqRunnerTest

printResult :: (String, Either String ()) -> IO ()
printResult (name, Left err) = putStrLn $ "  " ++ name ++ " - Error: " ++ err
printResult (name, Right ()) = putStrLn $ "  " ++ name ++ " - Success"

main :: IO ()
main = do
  let xmlParserResult = runAll XmlParserTestModule
  putStrLn "xmlParserTests:"
  mapM_ printResult xmlParserResult

  let xqParserResult = runAll XqParserTestModule
  putStrLn "xqParserTests:"
  mapM_ printResult xqParserResult

  let xqRunnerResult = runAll XqRunnerTestModule
  putStrLn "xqRunnerTests:"
  mapM_ printResult xqRunnerResult

  let allResults = snd <$> concat [xmlParserResult, xqParserResult, xqRunnerResult]
  let passedCount = length $ filter isRight allResults
  let failedCount = length $ filter isLeft allResults

  putStrLn $ "\n\nPassed: " ++ show passedCount ++ " Failed: " ++ show failedCount

  if failedCount == 0
    then exitSuccess
    else exitWith (ExitFailure 1)
