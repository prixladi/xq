module Tests.XQParser where

import Parser
import Tests.Helpers
import XQParser

data XQParserTestModule = XQParserTestModule

instance TestModule XQParserTestModule where
  runAll :: XQParserTestModule -> [(String, Either String ())]
  runAll _ =
    [ ("parseInvalidXQ", parseInvalidXQTest),
      ("parseXQWithRemainder", parseXQWithRemainderTest),
      ("parseEmptyXQ", parseEmptyXQTest),
      ("parseBasicXQ", parseBasicXQTest),
      ("parseComplexXQ", parseComplexXQTest)
    ]

parseInvalidXQTest :: Either String ()
parseInvalidXQTest = do
  let str = "/{book"
  expectFailure str

parseXQWithRemainderTest :: Either String ()
parseXQWithRemainderTest = do
  let str = "/book//{remainder}"
  let expected = [XQueryNode False (PreciseNode "book")]
  expectParsingRemainder str "//{remainder}" expected

parseEmptyXQTest :: Either String ()
parseEmptyXQTest = do
  let str = ""
  let expected = []
  expectSuccess str expected

parseBasicXQTest :: Either String ()
parseBasicXQTest = do
  let str = "/book"
  let expected = [XQueryNode False (PreciseNode "book")]
  expectSuccess str expected

parseComplexXQTest :: Either String ()
parseComplexXQTest = do
  let str = "/book//*//price/*"
  let expected = [XQueryNode False (PreciseNode "book"), XQueryNode True WildcardNode, XQueryNode True (PreciseNode "price"), XQueryNode False WildcardNode]
  expectSuccess str expected

expectSuccess :: String -> [XQValue] -> Either String ()
expectSuccess input expected = case runParser xQParser input of
  Just (rest, xq) -> if null rest then expectEq expected xq else Left $ "'" ++ rest ++ "' " ++ " remained after parsing XQuery"
  Nothing -> Left "Unable to parse XQuery"

expectFailure :: String -> Either String ()
expectFailure input = case runParser xQParser input of
  Just (_, xq) | xq /= [] -> Left $ "Expected parsing of XQ to fail, got: '" ++ show xq ++ "'"
  _ -> Right ()

expectParsingRemainder :: String -> String -> [XQValue] -> Either String ()
expectParsingRemainder input expectedRemainder expected = case runParser xQParser input of
  Just (rest, xq) -> expectEq expected xq >> expectEq expectedRemainder rest
  Nothing -> Left "Unable to parse XQ"