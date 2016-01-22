elm-abc-parser
==============

Work in progress. This is a parser for files conforming to the [ABC Notation (version 2.1)](http://abcnotation.com/wiki/abc:standard:v2.1) for traditional music scores. It is written in pure elm and uses the [elm-combine](https://github.com/Bogdanp/elm-combine) parser combinator library.  This is slightly difficult to achieve because, as far as I can make out, there is no formal grammar published, only a long textual description of the language.

To parse an ABC string you can use:

    abc
     |> parse

Dependencies
------------

*  Bogdanp/elm-combine  2.0.0
*  elm-lang/core 3.0.0

Limitations
-----------

So far, only ABC Headers have been implemented,
 




