module Test.XmlParser where

import Lib.Parser
import Lib.XmlParser
import Test.Helpers

data XmlParserTestModule = XmlParserTestModule

instance TestModule XmlParserTestModule where
  runAll :: XmlParserTestModule -> [(String, Either String ())]
  runAll _ =
    [ ("parseInvalidXml", parseInvalidXmlTest),
      ("parseXmlWithRemainder", parseXmlWithRemainderTest),
      ("parseBasicXml", parseBasicXmlTest),
      ("parseCommentedXml", parseCommentedXmlTest),
      ("parseXmlWithProlog", parseXmlWithPrologTest)
    ]

parseInvalidXmlTest :: Either String ()
parseInvalidXmlTest = do
  let str = "<bookstoree><book><price>1</price></book><book><price>2</price></book></bookstore>"
  expectParsingFailure str

parseXmlWithRemainderTest :: Either String ()
parseXmlWithRemainderTest = do
  let str = "<bookstore><book><price>1</price></book><book><__pr.....ice>2</__pr.....ice></book></bookstore>remainder"
  let expected = XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "__pr.....ice" [] [XmlContent "2"]]]
  expectParsingRemainder str "remainder" expected

parseBasicXmlTest :: Either String ()
parseBasicXmlTest = do
  let str = "<bookstore><book><price>1</price></book><book><price>2</price></book></bookstore>"
  let expected = XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"]]]
  expectParsingSuccess str expected

parseXmlWithPrologTest :: Either String ()
parseXmlWithPrologTest = do
  let str = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><bookstore><book><title lang=\"en\">Harry Potter</title><price>29.99</price></book><book><title lang=\"en\">Learning XML</title><price>39.95</price></book><price>88888888</price><bookstore><book><title lang=\"en\">LOTR</title><price>29.99</price></book><book><title lang=\"en\">Teaching JSON</title><price>39.95</price></book><price>88888888</price></bookstore></bookstore>"
  let expected = XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "title" [("lang", "en")] [XmlContent "Harry Potter"], XmlNode "price" [] [XmlContent "29.99"]], XmlNode "book" [] [XmlNode "title" [("lang", "en")] [XmlContent "Learning XML"], XmlNode "price" [] [XmlContent "39.95"]], XmlNode "price" [] [XmlContent "88888888"], XmlNode "bookstore" [] [XmlNode "book" [] [XmlNode "title" [("lang", "en")] [XmlContent "LOTR"], XmlNode "price" [] [XmlContent "29.99"]], XmlNode "book" [] [XmlNode "title" [("lang", "en")] [XmlContent "Teaching JSON"], XmlNode "price" [] [XmlContent "39.95"]], XmlNode "price" [] [XmlContent "88888888"]]]
  expectParsingSuccess str expected

parseCommentedXmlTest :: Either String ()
parseCommentedXmlTest = do
  let str = "<bookstore><book><!--testTest--><price>1</price></book><book><price>2</price></book></bookstore>"
  let expected = XmlNode "bookstore" [] [XmlNode "book" [] [XmlComment "testTest",XmlNode "price" [] [XmlContent "1"]], XmlNode "book" [] [XmlNode "price" [] [XmlContent "2"]]]
  expectParsingSuccess str expected

expectParsingSuccess :: String -> XmlValue -> Either String ()
expectParsingSuccess input expected = case runParser xmlParser input of
  Just (rest, xml) -> if null rest then expectEq expected xml else Left $ "'" ++ rest ++ "' " ++ " remained after parsing XML"
  Nothing -> Left "Unable to parse XML"

expectParsingFailure :: String -> Either String ()
expectParsingFailure input = case runParser xmlParser input of
  Just (_, xml) -> Left $ "Expected parsing of XML to fail, got: '" ++ show xml ++ "'"
  Nothing -> Right ()

expectParsingRemainder :: String -> String -> XmlValue -> Either String ()
expectParsingRemainder input expectedRemainder expected = case runParser xmlParser input of
  Just (rest, xml) -> expectEq expected xml >> expectEq expectedRemainder rest
  Nothing -> Left "Unable to parse XML"
