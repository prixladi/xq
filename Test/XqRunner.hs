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
      ("runXqWithSelectors3", runXqWithSelectorsTest3)
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

