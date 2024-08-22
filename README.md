# xq

`xq` is a command-line utility for querying `XML`. It is written in `Haskell` and has zero dependencies. It uses `XQuery` (a subset of the language `XPath`) for querying, read more about `XQuery` in the following sections.

## Project structure

- `/app` - Command line application entry point
- `/lib` - Core functionality of `xq`
- `/test` - Unit tests

## Building & Installing

`xq` requires:

- [GHC](https://www.haskell.org/ghc/) (Compiler)
- [Cabal](https://www.haskell.org/cabal/) (Build system)
- [Stack](https://docs.haskellstack.org/en/stable/#__tabbed_1_1) (Build system)

You can run your program directly using stack using:

```sh
stack run
```

This is just for development and testing purposes. If you want to install xq on your system or run benchmarks you should build the binary:

```sh
stack build
```

and copy it to your `/bin` folder, eg.:

```sh
cp $(stack path --local-install-root)/bin/xq-exe ~/bin/xq
```

## Usage

The first argument of xq is always `XQuery`, the second argument is either path to XML file `xq <xQuery> <xmlFilePath>` or is not present at all and XML must be provided in stdin `xq <xQuery> {xmlStdin}`.

File example:

```sh
xq "//book/*" "./bookstore.xml"
```

Stdin example:

```sh
cat "./bookstore.xml" | xq "//book/*"
```

## XQuery

`XQuery` (also referenced as `XQ`) is a language used for querying parsed `XML`. It is a small subset of the `XPath` language and aims to be completely replaced by `XPath` if every functionality gets implemented.

### Supported features

Supported features can be seen in the examples below. More exhaustive documentation is currently in progress. Right now you can check source code directly if you need specific. Parsing of the XQuery can be found in the [Parser file](Lib/XqParser.hs) and usage in querying XML can be found in the [Runner file](Lib/XqRunner.hs).

### Examples

**Descendant selectors**

Descendant selectors must start with '//' or '/'. The relative syntax without the **slash** qualifier is not supported.

| XQuery    | CSS selector equivalent |
| --------- | ----------------------- |
| //div     | div                     |
| //div//a  | div a                   |
| //div//\* | div \*                  |
| //div/\*  | div > \*                |
| /body     | :root > body            |
| /\*       | :root                   |

**Attribute selectors**

| XQuery                  | CSS selector equivalent |
| ----------------------- | ----------------------- |
| //input[@type='submit'] | input[type='submit']    |
| //a[@rel]               | a[rel]                  |

**Position selectors**
| XQuery | CSS selector equivalent |
| -------------------- | ----------------------- |
| //ul/li[position()=1] | ul > li:first-of-type |
| //ul/li[last()] | ul > li:last-of-type |
| //ul[@test='true']/li[@test='true'][last()] | ul[test='true'] > li[test='true']:last-of-type |

**Content selectors**

For number comparisons currently only the integer types are supported. A floating point number will result in an XQuery parse error.

| XQuery                     | Description                                                                        |
| -------------------------- | ---------------------------------------------------------------------------------- |
| //price[text()='100 EUR']  | Selects all price nodes that have content that equals to string "100 EUR"          |
| //price[text()!='100 EUR'] | Selects all price nodes that have content that does not equal to string "100 EUR"  |
| //price[text()>100]        | Selects all price nodes that have content that is parsable to int greater than 100 |
| //price[text()=100]        | Selects all price nodes that have content that is parsable to int equal 100        |

**Child selectors**

Child selectors allow to nest the additional XQuery into the selector and its result is treated as a boolean value. Child selectors support the same syntax as root XQuery with one exception - the first descendant selector must be relative (must start directly with tag without the **/** or **//** prefix)

| XQuery                                               | Description                                                                                                                                     |
| ---------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| //book[price[text()<500]]/title                      | Selects all titles of boots that have a price that is a number lesser than 500                                                               |
| //bookstore[book[@lang='en']/genre[text()>'comedy']] | Selects all bookstores that have at least one book that has attribute 'lang' set to 'en' and has genre subnode that contains the text 'comedy'. |

## Known issues and limitations

Some of the issues and limitations that are known to the maintainer/developers.

1. **Missing error hints** - _When parsing of the XML or XQ fails there are no error hints of which line and character the error occurred._
2. **XML parsing** -

   - _Parsing of the XML is not as strict as it should be. Such as:_

     - _XML tag with duplicate attributes is not considered invalid but it should be._
     - _Tags that start with the string **'xml'** are considered valid_
     - _XML prelude is not validated at all and can be included anywhere in the document_
     - _XML tag/attribute can contain multiple namespace indicators and can end with a namespace indicator (:)_
     - _And many more ..._

     _But this should not be so much of an issue since **xq** primary use is querying and not validating the XML_

   - _On the other hand, many of the XML valid constructs are not supported. Such as:_

     - _DTD._
     - _CDATA_
     - _Single quotes for attribute definitions_
     - _And many more ..._

## Unit tests

Basic `xq` use cases are written as tests in folder `/test`.

The tests can be started using

```sh
stack test
```
