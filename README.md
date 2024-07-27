# xq

`xq` is a command-line utility for querying `XML`. It is written in `Haskell` and has zero dependencies. It uses `XQuery` (a subset of the language `XPath`) for querying, read more about `XQuery` in the following sections.

## Project structure

- `/App` - Command line application entrypoint
- `/Lib` - Core functionality of `xq`
- `/Test` - Unit tests

## Building & Installing

`xq` does not have any dependency so all you need is a [Compiler](https://www.haskell.org/ghc/).

The following command will build the project into the `.output` folder:

```sh
ghc ./App/Main.hs -main-is App.Main -outputdir "./.output" -o "./.output/xq"
```

This will create program binary `./.output/xq` that can be moved into your bin directory, eg.

```sh
mv ./.output/xq ~/bin/
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

`XQuery` (also referenced as `XQ`) is a language used for querying parsed `XML`. It is a small subset of the `XPath` language and aims to be completely replaced by `XPath` if every functionality of it gets implemented.

## Known issues and limitations

Some of the issues and limitations that are known to the maintainer/developers.

1. **Missing error hints** - _When parsing of the XML or XQ fails there are no error hints of which line and character the error occurred._
2. **Duplicate attributes** - _XML tag with duplicate attributes is not considered invalid but it should be._
3. **Processing Instruction** - _XML document that contains Processing Instruction is considered invalid._
3. **DTD** - _XML document that contains DTD is considered invalid._

## Unit tests

Basic `xq` use cases are written as tests in folder `/Test`.

The tests can be built using

```sh
ghc ./Test/Main.hs -main-is Test.Main -outputdir "./.output" -o "./.output/test"
```

command and subsequently run using

```sh
./.output/test
```
