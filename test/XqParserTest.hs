{-# LANGUAGE InstanceSigs #-}

module XqParserTest (XqParserTestModule (..)) where

import Helpers
import Parser
import XqParser

data XqParserTestModule = XqParserTestModule

instance TestModule XqParserTestModule where
  runAll :: XqParserTestModule -> [(String, Either String ())]
  runAll _ =
    [ ("parseInvalidXq1", parseInvalidXqTest1),
      ("parseInvalidXq2", parseInvalidXqTest2),
      ("parseInvalidXq3", parseInvalidXqTest3),
      ("parseInvalidXqTest4", parseInvalidXqTest4),
      ("parseXqWithRemainder1", parseXqWithRemainderTest1),
      ("parseXqWithRemainder2", parseXqWithRemainderTest2),
      ("parseEmptyXq", parseEmptyXqTest),
      ("parseBasicXq1", parseBasicXqTest1),
      ("parseBasicXq2", parseBasicXqTest2),
      ("parseXqWithPositionSelector1", parseXqWithPositionSelectorTest1),
      ("parseXqWithPositionSelector2", parseXqWithPositionSelectorTest2),
      ("parseXqWithPositionSelector2", parseXqWithPositionSelectorTest3),
      ("parseXqWithAttributeSelector1", parseXqWithAttributeSelectorTest1),
      ("parseXqWithAttributeSelector2", parseXqWithAttributeSelectorTest2),
      ("parseXqWithContentSelector1", parseXqWithContentSelectorTest1),
      ("parseXqWithContentSelector2", parseXqWithContentSelectorTest2),
      ("parseXqWithContentSelector3", parseXqWithContentSelectorTest3),
      ("parseXqWithContentSelector4", parseXqWithContentSelectorTest4),
      ("parseXqWithChildSelector1", parseXqWithChildSelectorTest1),
      ("parseXqWithChildSelector2", parseXqWithChildSelectorTest2)
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

parseInvalidXqTest4 :: Either String ()
parseInvalidXqTest4 = do
  let str = "/[title[@lang='en']][price[text()<5000]"

  expectFailure str

parseXqWithRemainderTest1 :: Either String ()
parseXqWithRemainderTest1 = do
  let str = "/book//{remainder}"
  let expected = [XqNode False [XqTag $ PreciseTag "book"]]

  expectParsingRemainder str "//{remainder}" expected

parseXqWithRemainderTest2 :: Either String ()
parseXqWithRemainderTest2 = do
  let str = "/bookstore/book[title[@lang='en']][price[text()<5000]"
  let expected = [XqNode False [XqTag (PreciseTag "bookstore")], XqNode False [XqTag (PreciseTag "book"), XqChild [XqNode False [XqTag (PreciseTag "title"), XqAttribute (BasicAttribute "lang" (Just "en"))]]]]

  expectParsingRemainder str "[price[text()<5000]" expected

parseEmptyXqTest :: Either String ()
parseEmptyXqTest = do
  let str = ""
  let expected = []

  expectSuccess str expected

parseBasicXqTest1 :: Either String ()
parseBasicXqTest1 = do
  let str = "/book"
  let expected = [XqNode False [XqTag $ PreciseTag "book"]]

  expectSuccess str expected

parseBasicXqTest2 :: Either String ()
parseBasicXqTest2 = do
  let str = "/book//*//price/*"
  let expected = [XqNode False [XqTag $ PreciseTag "book"], XqNode True [XqTag WildcardTag], XqNode True [XqTag $ PreciseTag "price"], XqNode False [XqTag WildcardTag]]

  expectSuccess str expected

parseXqWithPositionSelectorTest1 :: Either String ()
parseXqWithPositionSelectorTest1 = do
  let str = "/book[position()>3]"
  let expected = [XqNode False [XqTag $ PreciseTag "book", XqPosition $ PrecisePosition Gt 3]]

  expectSuccess str expected

parseXqWithPositionSelectorTest2 :: Either String ()
parseXqWithPositionSelectorTest2 = do
  let str = "//*[last()]"
  let expected = [XqNode True [XqTag WildcardTag, XqPosition LastPosition]]

  expectSuccess str expected

parseXqWithPositionSelectorTest3 :: Either String ()
parseXqWithPositionSelectorTest3 = do
  let str = "//*/*[position()>3]"
  let expected = [XqNode True [XqTag WildcardTag], XqNode False [XqTag WildcardTag, XqPosition $ PrecisePosition Gt 3]]

  expectSuccess str expected

parseXqWithAttributeSelectorTest1 :: Either String ()
parseXqWithAttributeSelectorTest1 = do
  let str = "/*[@id]"
  let expected = [XqNode False [XqTag WildcardTag, XqAttribute (BasicAttribute "id" Nothing)]]

  expectSuccess str expected

parseXqWithAttributeSelectorTest2 :: Either String ()
parseXqWithAttributeSelectorTest2 = do
  let str = "/*[@id]/book[@id='12']"
  let expected = [XqNode False [XqTag WildcardTag, XqAttribute (BasicAttribute "id" Nothing)], XqNode False [XqTag (PreciseTag "book"), XqAttribute (BasicAttribute "id" (Just "12"))]]

  expectSuccess str expected

parseXqWithContentSelectorTest1 :: Either String ()
parseXqWithContentSelectorTest1 = do
  let str = "/bookstore/book/title[text()='Haskell']"
  let expected = [XqNode False [XqTag (PreciseTag "bookstore")], XqNode False [XqTag (PreciseTag "book")], XqNode False [XqTag (PreciseTag "title"), XqContent (StringContent Eq "Haskell")]]

  expectSuccess str expected

parseXqWithContentSelectorTest2 :: Either String ()
parseXqWithContentSelectorTest2 = do
  let str = "/bookstore/book/price[text()>100]"
  let expected = [XqNode False [XqTag (PreciseTag "bookstore")], XqNode False [XqTag (PreciseTag "book")], XqNode False [XqTag (PreciseTag "price"), XqContent (NumberContent Gt 100)]]

  expectSuccess str expected

parseXqWithContentSelectorTest3 :: Either String ()
parseXqWithContentSelectorTest3 = do
  let str = "/bookstore/book/price[text()!=1]"
  let expected = [XqNode False [XqTag (PreciseTag "bookstore")], XqNode False [XqTag (PreciseTag "book")], XqNode False [XqTag (PreciseTag "price"), XqContent (NumberContent NotEq 1)]]

  expectSuccess str expected

parseXqWithContentSelectorTest4 :: Either String ()
parseXqWithContentSelectorTest4 = do
  let str = "/bookstore[text()!='corner']/book/price[text()<1000]"
  let expected = [XqNode False [XqTag (PreciseTag "bookstore"), XqContent (StringContent NotEq "corner")], XqNode False [XqTag (PreciseTag "book")], XqNode False [XqTag (PreciseTag "price"), XqContent (NumberContent Lt 1000)]]

  expectSuccess str expected

parseXqWithChildSelectorTest1 :: Either String ()
parseXqWithChildSelectorTest1 = do
  let str = "/bookstore[book/price[text()<1000]]/name"
  let expected = [XqNode False [XqTag (PreciseTag "bookstore"), XqChild [XqNode False [XqTag (PreciseTag "book")], XqNode False [XqTag (PreciseTag "price"), XqContent (NumberContent Lt 1000)]]], XqNode False [XqTag (PreciseTag "name")]]

  expectSuccess str expected

parseXqWithChildSelectorTest2 :: Either String ()
parseXqWithChildSelectorTest2 = do
  let str = "/bookstore/book[title[@lang='en']][price[text()<5000]]"
  let expected = [XqNode False [XqTag (PreciseTag "bookstore")], XqNode False [XqTag (PreciseTag "book"), XqChild [XqNode False [XqTag (PreciseTag "title"), XqAttribute (BasicAttribute "lang" (Just "en"))]], XqChild [XqNode False [XqTag (PreciseTag "price"), XqContent (NumberContent Lt 5000)]]]]

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
