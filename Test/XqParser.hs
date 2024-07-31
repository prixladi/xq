module Test.XqParser where

import Lib.Parser
import Lib.XqParser
import Test.Helpers

data XqParserTestModule = XqParserTestModule

instance TestModule XqParserTestModule where
  runAll :: XqParserTestModule -> [(String, Either String ())]
  runAll _ =
    [ ("parseInvalidXq1", parseInvalidXqTest1),
      ("parseInvalidXq2", parseInvalidXqTest2),
      ("parseInvalidXq3", parseInvalidXqTest3),
      ("parseXqWithRemainder", parseXqWithRemainderTest),
      ("parseEmptyXq", parseEmptyXqTest),
      ("parseBasicXq1", parseBasicXqTest1),
      ("parseBasicXq2", parseBasicXqTest2),
      ("parseXqWithSelector1", parseXqWithSelectorTest1),
      ("parseXqWithSelector2", parseXqWithSelectorTest2),
      ("parseXqWithSelector3", parseXqWithSelectorTest3),
      ("parseXqWithSelector4", parseXqWithSelectorTest4),
      ("parseXqWithSelector5", parseXqWithSelectorTest5)
    ]

parseInvalidXqTest1 :: Either String ()
parseInvalidXqTest1 = do
  let str = "/{book"
  expectFailure str

parseInvalidXqTest2 :: Either String ()
parseInvalidXqTest2 = do
  let str = "//"
  expectFailure str

parseInvalidXqTest3 :: Either String ()
parseInvalidXqTest3 = do
  let str = "//[llast()]"
  expectFailure str

parseXqWithRemainderTest :: Either String ()
parseXqWithRemainderTest = do
  let str = "/book//{remainder}"
  let expected = [XqNode False [Tag $ PreciseTag "book"]]
  expectParsingRemainder str "//{remainder}" expected

parseEmptyXqTest :: Either String ()
parseEmptyXqTest = do
  let str = ""
  let expected = []
  expectSuccess str expected

parseBasicXqTest1 :: Either String ()
parseBasicXqTest1 = do
  let str = "/book"
  let expected = [XqNode False [Tag $ PreciseTag "book"]]
  expectSuccess str expected

parseBasicXqTest2 :: Either String ()
parseBasicXqTest2 = do
  let str = "/book//*//price/*"
  let expected = [XqNode False [Tag $ PreciseTag "book"], XqNode True [Tag WildcardTag], XqNode True [Tag $ PreciseTag "price"], XqNode False [Tag WildcardTag]]
  expectSuccess str expected

parseXqWithSelectorTest1 :: Either String ()
parseXqWithSelectorTest1 = do
  let str = "/book[position()>3]"
  let expected = [XqNode False [Tag $ PreciseTag "book", Position $ PrecisePosition Gt 3]]
  expectSuccess str expected

parseXqWithSelectorTest2 :: Either String ()
parseXqWithSelectorTest2 = do
  let str = "//*[last()]"
  let expected = [XqNode True [Tag WildcardTag, Position LastPosition]]
  expectSuccess str expected

parseXqWithSelectorTest3 :: Either String ()
parseXqWithSelectorTest3 = do
  let str = "//*/*[position()>3]"
  let expected = [XqNode True [Tag WildcardTag], XqNode False [Tag WildcardTag, Position $ PrecisePosition Gt 3]]
  expectSuccess str expected

parseXqWithSelectorTest4 :: Either String ()
parseXqWithSelectorTest4 = do
  let str = "/*[@id]"
  let expected = [XqNode False [Tag WildcardTag, Attribute (BasicAttribute "id" Nothing)]]
  expectSuccess str expected

parseXqWithSelectorTest5 :: Either String ()
parseXqWithSelectorTest5 = do
  let str = "/*[@id]/book[@id='12']"
  let expected = [XqNode False [Tag WildcardTag, Attribute (BasicAttribute "id" Nothing)], XqNode False [Tag (PreciseTag "book"), Attribute (BasicAttribute "id" (Just "12"))]]
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
