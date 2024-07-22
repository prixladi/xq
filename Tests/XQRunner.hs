module Tests.XqRunner where

import Parser
import Tests.Helpers
import XqParser
import XqRunner
import XmlParser

data XqRunnerTestModule = XqRunnerTestModule

instance TestModule XqRunnerTestModule where
  runAll :: XqRunnerTestModule -> [(String, Either String ())]
  runAll _ =
    [ ("runEmptyXq", runEmptyXqtest),
      ("runEmptyResultXq", runEmptyResultXqtest),
      ("runBasicXq1", runBasicXqtest1),
      ("runBasicXq2", runBasicXqtest2)
    ]

runEmptyXqtest :: Either String ()
runEmptyXqtest = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = []
  let expected = [XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]]

  expectEq expected (runXq xq xml)

runEmptyResultXqtest :: Either String ()
runEmptyResultXqtest = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode False (PreciseNode "bookstore"), XqNode False (PreciseNode "book"), XqNode False WildcardNode, XqNode False (PreciseNode "book")]
  let expected = []

  expectEq expected (runXq xq xml)

runBasicXqtest1 :: Either String ()
runBasicXqtest1 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode False (PreciseNode "bookstore"), XqNode False (PreciseNode "book"), XqNode False WildcardNode]
  let expected = [XmlNode "price" [] [XmlContent "1"], XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]

  expectEq expected (runXq xq xml)

runBasicXqtest2 :: Either String ()
runBasicXqtest2 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XqNode True (PreciseNode "book")]
  let expected = [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]

  expectEq expected (runXq xq xml)
