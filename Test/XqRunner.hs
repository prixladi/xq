module Test.XqRunner where

import Parser
import Test.Helpers
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
      ("runBasicXq2", runBasicXqTest2)
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
  let xq = [XqNode False (PreciseNode "bookstore"), XqNode False (PreciseNode "book"), XqNode False WildcardNode, XqNode False (PreciseNode "book")]
  let expected = []

  expectEq expected (runXq xq xml)

runBasicXqTest1 :: Either String ()
runBasicXqTest1 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode False (PreciseNode "bookstore"), XqNode False (PreciseNode "book"), XqNode False WildcardNode]
  let expected = [XmlNode "price" [] [XmlContent "1"], XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]

  expectEq expected (runXq xq xml)

runBasicXqTest2 :: Either String ()
runBasicXqTest2 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode True (PreciseNode "book")]
  let expected = [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]

  expectEq expected (runXq xq xml)
