{-# LANGUAGE InstanceSigs #-}

module XqRunnerTest (XqRunnerTestModule (..)) where

import Helpers
import XmlParser
import XqParser
import XqRunner

data XqRunnerTestModule = XqRunnerTestModule

instance TestModule XqRunnerTestModule where
  runAll :: XqRunnerTestModule -> [(String, Either String ())]
  runAll _ =
    [ ("runEmptyXq", runEmptyXqTest),
      ("runEmptyResultXq", runEmptyResultXqTest),
      ("runBasicXq1", runBasicXqTest1),
      ("runBasicXq2", runBasicXqTest2),
      ("runXqWithPositionSelectors1", runXqWithPositionSelectorsTest1),
      ("runXqWithPositionSelectors2", runXqWithPositionSelectorsTest2),
      ("runXqWithPositionSelectors3", runXqWithPositionSelectorsTest3),
      ("runXqWithAttributeSelectors1", runXqWithAttributeSelectorsTest1),
      ("runXqWithAttributeSelectors2", runXqWithAttributeSelectorsTest2),
      ("runXqWithAttributeSelectors3", runXqWithAttributeSelectorsTest3),
      ("runXqWithAttributeSelectors4", runXqWithAttributeSelectorsTest4),
      ("runXqWithContentSelectors1", runXqWithContentSelectorsTest1),
      ("runXqWithContentSelectors2", runXqWithContentSelectorsTest2),
      ("runXqWithChildSelectors1", runXqWithChildSelectorsTest1)
    ]

runEmptyXqTest :: Either String ()
runEmptyXqTest = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = []
  let expected = [XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]]

  expectEq expected (runXq xq xml)

runEmptyResultXqTest :: Either String ()
runEmptyResultXqTest = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode False [XqTag $ PreciseTag "bookstore"], XqNode False [XqTag $ PreciseTag "book"], XqNode False [XqTag $ WildcardTag], XqNode False [XqTag $ PreciseTag "book"]]
  let expected = []

  expectEq expected (runXq xq xml)

runBasicXqTest1 :: Either String ()
runBasicXqTest1 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode False [XqTag $ PreciseTag "bookstore"], XqNode False [XqTag $ PreciseTag "book"], XqNode False [XqTag $ WildcardTag]]
  let expected = [XmlNode "price" [] [XmlContent "1"], XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]

  expectEq expected (runXq xq xml)

runBasicXqTest2 :: Either String ()
runBasicXqTest2 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode True [XqTag $ PreciseTag "book"]]
  let expected = [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]

  expectEq expected (runXq xq xml)

runXqWithPositionSelectorsTest1 :: Either String ()
runXqWithPositionSelectorsTest1 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode True [XqTag $ PreciseTag "book", XqPosition $ PrecisePosition Eq 1]]
  let expected = [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]]]

  expectEq expected (runXq xq xml)

runXqWithPositionSelectorsTest2 :: Either String ()
runXqWithPositionSelectorsTest2 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode True [XqTag $ PreciseTag "book", XqPosition $ PrecisePosition Eq 5]]
  let expected = []

  expectEq expected (runXq xq xml)

runXqWithPositionSelectorsTest3 :: Either String ()
runXqWithPositionSelectorsTest3 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode True [XqTag $ PreciseTag "book", XqPosition LastPosition]]
  let expected = [XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]

  expectEq expected (runXq xq xml)

runXqWithAttributeSelectorsTest1 :: Either String ()
runXqWithAttributeSelectorsTest1 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [("lang", "en")] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [("lang", "de")] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode True [XqTag WildcardTag, XqAttribute $ BasicAttribute "lang" Nothing]]
  let expected = [XmlNode "book" [("lang", "en")] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [("lang", "de")] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]

  expectEq expected (runXq xq xml)

runXqWithAttributeSelectorsTest2 :: Either String ()
runXqWithAttributeSelectorsTest2 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [("lang", "en")] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [("lang", "de")] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode True [XqTag WildcardTag, XqAttribute $ BasicAttribute "lang" Nothing, XqPosition LastPosition]]
  let expected = [XmlNode "book" [("lang", "de")] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]

  expectEq expected (runXq xq xml)

runXqWithAttributeSelectorsTest3 :: Either String ()
runXqWithAttributeSelectorsTest3 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [("lang", "en")] [XmlNode "price" [] [XmlContent "1", XmlProcessingInstruction "pi aaaa ?"]], XmlProcessingInstruction "pi aaaa ?", XmlNode "book" [("lang", "de")] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode True [XqTag WildcardTag, XqAttribute $ BasicAttribute "lang" (Just "de")]]
  let expected = [XmlNode "book" [("lang", "de")] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]

  expectEq expected (runXq xq xml)

runXqWithAttributeSelectorsTest4 :: Either String ()
runXqWithAttributeSelectorsTest4 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [("lang", "en")] [XmlNode "price" [] [XmlContent "1", XmlProcessingInstruction "pi aaaa ?"], XmlProcessingInstruction "pi aaaa ?"], XmlNode "book" [("lang", "de")] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode True [XqTag WildcardTag, XqAttribute $ BasicAttribute "lang" Nothing], XqNode False [XqTag $ PreciseTag "price"]]
  let expected = [XmlNode "price" [] [XmlContent "1", XmlProcessingInstruction "pi aaaa ?"], XmlNode "price" [] [XmlContent "2"]]

  expectEq expected (runXq xq xml)

runXqWithContentSelectorsTest1 :: Either String ()
runXqWithContentSelectorsTest1 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [("lang", "en")] [XmlNode "price" [] [XmlContent "123", XmlProcessingInstruction "pi aaaa ?"], XmlProcessingInstruction "pi aaaa ?"], XmlNode "book" [("lang", "de")] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode True [XqTag (PreciseTag "book")], XqNode False [XqTag (PreciseTag "price"), XqContent (NumberContent Eq 123)]]
  let expected = [XmlNode "price" [] [XmlContent "123", XmlProcessingInstruction "pi aaaa ?"]]

  expectEq expected (runXq xq xml)

runXqWithContentSelectorsTest2 :: Either String ()
runXqWithContentSelectorsTest2 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [("lang", "en")] [XmlContent "Haskell", XmlNode "price" [] [XmlContent "123"]], XmlNode "book" [("lang", "en")] [XmlContent "Haskell", XmlNode "price" [] [XmlContent "11", XmlProcessingInstruction "pi aaaa ?"], XmlProcessingInstruction "pi aaaa ?"], XmlNode "book" [("lang", "de")] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode True [XqTag (PreciseTag "book"), XqContent $ StringContent Eq "Haskell"], XqNode False [XqTag (PreciseTag "price"), XqContent (NumberContent Gt 12)]]
  let expected = [XmlNode "price" [] [XmlContent "123"]]

  expectEq expected (runXq xq xml)

runXqWithChildSelectorsTest1 :: Either String ()
runXqWithChildSelectorsTest1 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [("lang", "en")] [XmlNode "price" [] [XmlContent "310"]], XmlNode "book" [("lang", "de")] [XmlNode "price" [] [XmlContent "125"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode True [XqTag (PreciseTag "book"), XqChild [XqNode False [XqTag $ PreciseTag "price", XqContent $ NumberContent Lt 200]]]]
  let expected = [XmlNode "book" [("lang", "de")] [XmlNode "price" [] [XmlContent "125"], XmlNode "title" [] [XmlContent "jack"]]]

  expectEq expected (runXq xq xml)