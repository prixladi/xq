module Tests.XMLParser where

import Parser
import Tests.Helpers
import XMLParser

data XMLParserTestModule = XMLParserTestModule

instance TestModule XMLParserTestModule where
  runAll :: XMLParserTestModule -> [(String, Either String ())]
  runAll _ =
    [ ("parseInvalidXml", parseInvalidXmlTest),
      ("parseXmlWithRemainder", parseXmlWithRemainderTest),
      ("parseBasicXml", parseBasicXmlTest),
      ("parseComplexXml", parseComplexXmlTest)
    ]

parseInvalidXmlTest :: Either String ()
parseInvalidXmlTest = do
  let str = "<bookstoree><book><price>1</price></book><book><price>2</price></book></bookstore>"
  expectParsingFailure str

parseXmlWithRemainderTest :: Either String ()
parseXmlWithRemainderTest = do
  let str = "<bookstore><book><price>1</price></book><book><price>2</price></book></bookstore>remainder"
  let expected = XMLNode "bookstore" [] [XMLNode "book" [] [XMLNode "price" [] [XMLContent "1"]], XMLNode "book" [] [XMLNode "price" [] [XMLContent "2"]]]
  expectParsingRemainder str "remainder" expected

parseBasicXmlTest :: Either String ()
parseBasicXmlTest = do
  let str = "<bookstore><book><price>1</price></book><book><price>2</price></book></bookstore>"
  let expected = XMLNode "bookstore" [] [XMLNode "book" [] [XMLNode "price" [] [XMLContent "1"]], XMLNode "book" [] [XMLNode "price" [] [XMLContent "2"]]]
  expectParsingSuccess str expected

parseComplexXmlTest :: Either String ()
parseComplexXmlTest = do
  let str = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><bookstore><book><title lang=\"en\">Harry Potter</title><price>29.99</price></book><book><title lang=\"en\">Learning XML</title><price>39.95</price></book><price>88888888</price><bookstore><book><title lang=\"en\">LOTR</title><price>29.99</price></book><book><title lang=\"en\">Teaching JSON</title><price>39.95</price></book><price>88888888</price></bookstore></bookstore>"
  let expected = XMLNode "bookstore" [] [XMLNode "book" [] [XMLNode "title" [("lang", "en")] [XMLContent "Harry Potter"], XMLNode "price" [] [XMLContent "29.99"]], XMLNode "book" [] [XMLNode "title" [("lang", "en")] [XMLContent "Learning XML"], XMLNode "price" [] [XMLContent "39.95"]], XMLNode "price" [] [XMLContent "88888888"], XMLNode "bookstore" [] [XMLNode "book" [] [XMLNode "title" [("lang", "en")] [XMLContent "LOTR"], XMLNode "price" [] [XMLContent "29.99"]], XMLNode "book" [] [XMLNode "title" [("lang", "en")] [XMLContent "Teaching JSON"], XMLNode "price" [] [XMLContent "39.95"]], XMLNode "price" [] [XMLContent "88888888"]]]
  expectParsingSuccess str expected

expectParsingSuccess :: String -> XMLValue -> Either String ()
expectParsingSuccess input expected = case runParser xmlParser input of
  Just (rest, xml) -> if null rest then expectEq expected xml else Left $ "'" ++ rest ++ "' " ++ " remained after parsing XML"
  Nothing -> Left "Unable to parse XML"

expectParsingFailure :: String -> Either String ()
expectParsingFailure input = case runParser xmlParser input of
  Just (_, xml) -> Left $ "Expected parsing of XML to fail, got: '" ++ show xml ++ "'"
  Nothing -> Right ()

expectParsingRemainder :: String -> String -> XMLValue -> Either String ()
expectParsingRemainder input expectedRemainder expected = case runParser xmlParser input of
  Just (rest, xml) -> expectEq expected xml >> expectEq expectedRemainder rest
  Nothing -> Left "Unable to parse XML"
