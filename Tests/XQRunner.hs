module Tests.XQRunner where

import Parser
import Tests.Helpers
import XQParser
import XQRunner
import XmlParser

data XQRunnerTestModule = XQRunnerTestModule

instance TestModule XQRunnerTestModule where
  runAll :: XQRunnerTestModule -> [(String, Either String ())]
  runAll _ =
    [ ("runEmptyXQ", runEmptyXQtest),
      ("runEmptyResultXQ", runEmptyResultXQtest),
      ("runBasicXQ1", runBasicXQtest1),
      ("runBasicXQ2", runBasicXQtest2)
    ]

runEmptyXQtest :: Either String ()
runEmptyXQtest = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = []
  let expected = [XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]]

  expectEq expected (runXQ xq xml)

runEmptyResultXQtest :: Either String ()
runEmptyResultXQtest = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XQNode False (PreciseNode "bookstore"), XQNode False (PreciseNode "book"), XQNode False WildcardNode, XQNode False (PreciseNode "book")]
  let expected = []

  expectEq expected (runXQ xq xml)

runBasicXQtest1 :: Either String ()
runBasicXQtest1 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XQNode False (PreciseNode "bookstore"), XQNode False (PreciseNode "book"), XQNode False WildcardNode]
  let expected = [XmlNode "price" [] [XmlContent "1"], XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]

  expectEq expected (runXQ xq xml)

runBasicXQtest2 :: Either String ()
runBasicXQtest2 = do
  let xml = XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]
  let xq = [XQNode True (PreciseNode "book")]
  let expected = [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"], XmlNode "title" [] [XmlContent "jack"]]]

  expectEq expected (runXQ xq xml)
