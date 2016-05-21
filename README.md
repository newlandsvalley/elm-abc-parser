elm-abc-parser
==============

This is a parser for files conforming to the [ABC Notation (version 2.1)](http://abcnotation.com/wiki/abc:standard:v2.1) for traditional music scores. It is written in pure Elm (version 0.17) and uses the [elm-combine](https://github.com/Bogdanp/elm-combine) parser combinator library.  This is slightly difficult to achieve because, as far as I can make out, there is no formal grammar published, only a long textual description of the language.

The intention is to provide a fully conformant parser for individually presented monophonic ABC tunes.  The grammar has been slightly relaxed in places so that it can support an application that 'plays' either the entire tune or just a portion of the tune body.  So, for example, the rules that require the 'X' header to come first and the 'T' header to come second will be dropped.  Obviously it is not possible to play a tune fragment if headers representing key signature, note length and tempo are absent, so these are defaulted to sensible values if they don't exist.

To parse an ABC string you can use:

    abc
     |> parse 
     
Additional facilities exist in the [Music](https://github.com/newlandsvalley/elm-abc-parser/tree/master/src/Music) modules to manipulate the [parse tree](https://github.com/newlandsvalley/elm-abc-parser/blob/master/src/Abc/ParseTree.elm) in order (for example) to transpose a tune or change its octave.
     
Dependencies
------------

*  elm-lang/core 4.0.0
*  Bogdanp/elm-combine 2.2.1
*  imeckler/ratio 2.0.1
*  elm-community/maybe-extra 1.0.0
*  elm-community/elm-list-extra 1.0.0

Deviations from the Spec
------------------------

Sections of the spec marked as 'volatile' are treated as being non-normative.  The parser attempts to treat them as liberally as possible.  In any case, these sections do not affect the primary purpose which is to support the replay of simple monophonic tunes.

Implementation Notes
--------------------

Slurs are not implemented in a way that I like (where you recognize the left bracket, then the slurred notes and finally the right bracket; thus balancing the brackets).  This seems to be a limitation in the spec - this is written in a line-oriented fashion, where lines of notes may possibly be interrupted by in-line headers. However, in the wild (and perhaps the spec - it's unclear) slurs may occur across lines.  This means that all that can be easily recognised is the individual brackets '(' and ') that frame   the slur and which are now disconnected from each other in any meaningful sense.

Bar lines are specified very badly and the parser tries to make the best of a bad job.


 




