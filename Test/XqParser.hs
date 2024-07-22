module Test.XqParser where

import Parser
import Test.Helpers
import XqParser

data XqParserTestModule = XqParserTestModule

instance TestModule XqParserTestModule where
  runAll :: XqParserTestModule -> [(String, Either String ())]
  runAll _ =
    [ ("parseInvalidXq", parseInvalidXqTest),
      ("parseXqWithRemainder", parseXqWithRemainderTest),
      ("parseEmptyXq", parseEmptyXqTest),
      ("parseBasicXq", parseBasicXqTest),
      ("parseComplexXq", parseComplexXqTest)
    ]

parseInvalidXqTest :: Either String ()
parseInvalidXqTest = do
  let str = "/{book"
  expectFailure str

parseXqWithRemainderTest :: Either String ()
parseXqWithRemainderTest = do
  let str = "/book//{remainder}"
  let expected = [XqNode False (PreciseNode "book")]
  expectParsingRemainder str "//{remainder}" expected

parseEmptyXqTest :: Either String ()
parseEmptyXqTest = do
  let str = ""
  let expected = []
  expectSuccess str expected

parseBasicXqTest :: Either String ()
parseBasicXqTest = do
  let str = "/book"
  let expected = [XqNode False (PreciseNode "book")]
  expectSuccess str expected

parseComplexXqTest :: Either String ()
parseComplexXqTest = do
  let str = "/book//*//price/*"
  let expected = [XqNode False (PreciseNode "book"), XqNode True WildcardNode, XqNode True (PreciseNode "price"), XqNode False WildcardNode]
  expectSuccess str expected

expectSuccess :: String -> [XqValue] -> Either String ()
expectSuccess input expected = case runParser xqParser input of
  Just (rest, xq) -> if null rest then expectEq expected xq else Left $ "'" ++ rest ++ "' " ++ " remained after parsing Xq"
  Nothing -> Left "Unable to parse Xq"

expectFailure :: String -> Either String ()
expectFailure input = case runParser xqParser input of
  Just (_, xq) | xq /= [] -> Left $ "Expected parsing of Xq to fail, got: '" ++ show xq ++ "'"
  _ -> Right ()

expectParsingRemainder :: String -> String -> [XqValue] -> Either String ()
expectParsingRemainder input expectedRemainder expected = case runParser xqParser input of
  Just (rest, xq) -> expectEq expected xq >> expectEq expectedRemainder rest
  Nothing -> Left "Unable to parse Xq"