module Test.XqRunner where

import Lib.Parser
import Lib.XmlParser
import Lib.XqParser
import Lib.XqRunner
import Test.Helpers

data XqRunnerTestModule = XqRunnerTestModule

instance TestModule XqRunnerTestModule where
  runAll :: XqRunnerTestModule -> [(String, Either String ())]
  runAll _ =
    [ ("runEmptyXq", runEmptyXqTest),
      ("runEmptyResultXq", runEmptyResultXqTest),
      ("runBasicXq1", runBasicXqTest1),
      ("runBasicXq2", runBasicXqTest2),
      ("runXqWithSelectors1", runXqWithSelectorsTest1),
      ("runXqWithSelectors2", runXqWithSelectorsTest2),
      ("runXqWithSelectors3", runXqWithSelectorsTest3),
      ("runXqWithAttributeSelectors1", runXqWithAttributeSelectorsTest1),
      ("runXqWithAttributeSelectors2", runXqWithAttributeSelectorsTest2),
      ("runXqWithAttributeSelectors3", runXqWithAttributeSelectorsTest3),
      ("runXqWithAttributeSelectors4", runXqWithAttributeSelectorsTest4)
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
  let xq = [XqNode False [Tag $ PreciseTag "bookstore"], XqNode False [Tag $ PreciseTag "book"], XqNode False [Tag $ WildcardTag], XqNode False [Tag $ PreciseTag "book"]]
  let expected = []

  expectEq expected (runXq xq xml)

runBasicXqTest1 :: Either String ()
runBasicXqTest1 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode False [Tag $ PreciseTag "bookstore"], XqNode False [Tag $ PreciseTag "book"], XqNode False [Tag $ WildcardTag]]
  let expected = [XmlNode "price" [] [XmlContent "1"], XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]

  expectEq expected (runXq xq xml)

runBasicXqTest2 :: Either String ()
runBasicXqTest2 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode True [Tag $ PreciseTag "book"]]
  let expected = [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]

  expectEq expected (runXq xq xml)

runXqWithSelectorsTest1 :: Either String ()
runXqWithSelectorsTest1 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode True [Tag $ PreciseTag "book", Position $ PrecisePosition Eq 1]]
  let expected = [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]]]

  expectEq expected (runXq xq xml)

runXqWithSelectorsTest2 :: Either String ()
runXqWithSelectorsTest2 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode True [Tag $ PreciseTag "book", Position $ PrecisePosition Eq 5]]
  let expected = []

  expectEq expected (runXq xq xml)

runXqWithSelectorsTest3 :: Either String ()
runXqWithSelectorsTest3 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode True [Tag $ PreciseTag "book", Position LastPosition]]
  let expected = [XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]

  expectEq expected (runXq xq xml)

runXqWithAttributeSelectorsTest1 :: Either String ()
runXqWithAttributeSelectorsTest1 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [("lang", "en")] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [("lang", "de")] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode True [Tag WildcardTag, Attribute $ BasicAttribute "lang" Nothing]]
  let expected = [XmlNode "book" [("lang", "en")] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [("lang", "de")] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]

  expectEq expected (runXq xq xml)

runXqWithAttributeSelectorsTest2 :: Either String ()
runXqWithAttributeSelectorsTest2 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [("lang", "en")] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [("lang", "de")] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode True [Tag WildcardTag, Attribute $ BasicAttribute "lang" Nothing, Position LastPosition]]
  let expected = [XmlNode "book" [("lang", "de")] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]

  expectEq expected (runXq xq xml)

runXqWithAttributeSelectorsTest3 :: Either String ()
runXqWithAttributeSelectorsTest3 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [("lang", "en")] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [("lang", "de")] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode True [Tag WildcardTag, Attribute $ BasicAttribute "lang" (Just "de")]]
  let expected = [XmlNode "book" [("lang", "de")] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]

  expectEq expected (runXq xq xml)

runXqWithAttributeSelectorsTest4 :: Either String ()
runXqWithAttributeSelectorsTest4 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [("lang", "en")] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [("lang", "de")] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode True [Tag WildcardTag, Attribute $ BasicAttribute "lang" Nothing], XqNode False [Tag $ PreciseTag "price"]]
  let expected = [XmlNode "price" [] [XmlContent "1"],XmlNode "price" [] [XmlContent "2"]]

  expectEq expected (runXq xq xml)
