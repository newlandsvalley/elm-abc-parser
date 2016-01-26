elm-abc-parser
==============

Work in progress. This is a parser for files conforming to the [ABC Notation (version 2.1)](http://abcnotation.com/wiki/abc:standard:v2.1) for traditional music scores. It is written in pure elm and uses the [elm-combine](https://github.com/Bogdanp/elm-combine) parser combinator library.  This is slightly difficult to achieve because, as far as I can make out, there is no formal grammar published, only a long textual description of the language.

The intention is to provide a fully conformant parser for individually presented monophonic ABC tunes.  The grammar will be slightly relaxed in places so that it can support an application that 'plays' either the entire tune or just a portion of the tune body.  So, for example, the rules that require the 'X' header to come first and the 'T' header to come second will be dropped.  Obviously it is not possible to play a tune fragment if headers representing key signature, note length and tempo are absent, so these will have to be defaulted to sensible values if they don't exist.

To parse an ABC string you can use:

    abc
     |> parse
     
Dependencies
------------

*  Bogdanp/elm-combine 2.0.0
*  imeckler/ratio 2.0.0
*  circuithub/elm-maybe-extra 1.6.0
*  elm-lang/core 3.0.0

Limitations
-----------

To do:

*  Articulations
*  Annotations
*  Decorations
*  Tempo: common time
*  Beams: recognise backtick separators    
*  Ties: attach to leading note
    
 
 




