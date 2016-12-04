module Abc
    exposing
        ( parse
        , parseKeySignature
        , parseError
        , ParseError
        )

{-| Library for parsing tunes written in ABC notation -
     see: http://abcnotation.com/wiki/abc:standard:v2.1

# Functions
@docs parse, parseKeySignature, parseError

# Types
@docs ParseError

-}

import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Num exposing (..)


-- import Ratio exposing (Rational, over, fromInt, divide)

import String exposing (fromList, toList, foldl)
import Char exposing (fromCode, toCode, isUpper)
import Debug exposing (..)
import Maybe exposing (withDefault)
import Maybe.Extra exposing (join)
import Tuple exposing (first, second)
import Dict exposing (Dict, get)
import Result exposing (Result)
import Regex exposing (Regex, contains)
import Abc.ParseTree exposing (..)
import Ratio exposing (Rational, over, fromInt, divide)
import Combine.Extra exposing (manyTill1, leftBiasedOr)


{-| A parse error with context.
-}
type alias ParseError =
    { msgs : List String
    , input : String
    , position : Int
    }



-- top level parsers


abc : Parser s AbcTune
abc =
    (,) <$> headers <*> body



-- BODY


body : Parser s (List BodyPart)
body =
    (::)
        <$> score
        <*> manyTill1
                -- (score `leftBiasedOr` tuneBodyHeader)
                (leftBiasedOr score tuneBodyHeader)
                end


score : Parser s BodyPart
score =
    Score <$> manyTill1 scoreItem eol


scoreItem : Parser s Music
scoreItem =
    lazy <|
        \() ->
            -- log "score item" <$>
            (choice
                [ chord
                , inline
                , barline
                , brokenRhythmPair
                  -- must place before note because of potential ambiguity of AbcNote
                , note
                , rest
                , tuplet
                , slur
                , graceNote
                  -- we are not enforcing the ordering of grace notes, chords etc pre-note
                , annotation
                , chordSymbol
                , decoration
                , spacer
                , ignore
                , continuation
                ]
            )
                <?> "score item"


barline : Parser s Music
barline =
    choice
        [ normalBarline
        , degenerateBarRepeat
        ]



{- a normal bar line (plus optional repeat iteration marker)
   see comments in 4.8 Repeat/bar symbols:
   Abc parsers should be quite liberal in recognizing bar lines. In the wild, bar lines may have
   any shape, using a sequence of | (thin bar line), [| or |] (thick bar line), and : (dots), e.g. |[| or [|:::
-}


normalBarline : Parser s Music
normalBarline =
    buildBarline
        <$> barSeparator
        <*> maybe repeatSection
        <?> "barline"



{- sometimes in the wild we get a degenerate repeat marker at the start of a line of music like this:
     [1 .....
   or
     _[1 ....
   again we have to be careful about ambiguity between this and inline headers by making sure we parse '[' immediately followed by '1' etc.
-}


degenerateBarRepeat : Parser s Music
degenerateBarRepeat =
    Barline
        <$> (Bar Thin Nothing
                <$> (Just
                        <$> (whiteSpace *> char '[' *> Combine.Num.digit)
                    )
            )



{- written like this instead of a regex because it's all regex control character! -}


barSeparator : Parser s String
barSeparator =
    String.concat
        <$> (many1 <|
                choice
                    [ string "[|"
                    , string "|]:"
                      -- must come before |] otherwise it hides it
                    , string "|]"
                    , string "]|:"
                    , string "]|"
                    , string ":[|"
                    , string "|:"
                    , string ":|:"
                    , string ":||:"
                    , string ":|]"
                      -- must come before :| else it hides it
                    , string ":||"
                    , string ":|"
                    , string "::"
                    , string "||:"
                      -- must come before || else it hides it
                    , string "||"
                    , string "|"
                      -- must be last otherwise it hides |:
                    ]
            )



{- a repeat section at the start of a bar.  We have just parsed a bar marker (say |) and so the combination of this and the repeat may be:
      |1
      |[1
      | [1
   but is not allowed to be
      | 1

   associating the digit with the bracket bar number should remove ambiguity with respect to other productions that use the bracket
   (in particular, inline headers and chords).
-}


repeatSection : Parser s Int
repeatSection =
    choice
        [ Combine.Num.digit
        , whiteSpace *> char '[' *> Combine.Num.digit
        ]


slur : Parser s Music
slur =
    Slur
        <$> choice [ char '(', char ')' ]
        <?> "slur"



{- Note, Slur should really be defined as Slur (List Music) and then parsed as shown below.  This would allow slurs to be
      nested and the parser to test that the brackets are balanced.  However, unfortunately, in the wild there are examples
      of slurs that go across music lines which make this interpretation impossible.  We thus simply parse the bracket characters.

      lazy evaluation of this unimplemented slur - comments courtesy of Bogdanp.
      Elm is eagerly evaluated so slur and slurContent end up creating a circular dependency that gets evaluated as soon as the output JS does,
      which means one function ends up calling the constructor of the other before the other is defined. The rec combinator introduces
      laziness to get round the problem


   slur : Parser Music
   slur = rec <| \() ->
           Slur <$>  parens (many1 musicItem)
                <?> "slur"
-}
-- spec is unclear if spaces are allowed after a broken rhythm operator but it's easy to support, is more permissive and doesn't break anything


brokenRhythmTie : Parser s Broken
brokenRhythmTie =
    buildBrokenOperator <$> regex "(<+|>+)" <* whiteSpace


brokenRhythmPair : Parser s Music
brokenRhythmPair =
    BrokenRhythmPair
        <$> abcNote
        <*> brokenRhythmTie
        <*> abcNote
        <?> "broken rhythm pair"


rest : Parser s Music
rest =
    Rest
        <$> (withDefault (fromInt 1) <$> (regex "[XxZz]" *> maybe noteDur))
        <?> "rest"



{- a free - format chord symbol - see 4.18 Chord symbols -}


chordSymbol : Parser s Music
chordSymbol =
    ChordSymbol
        <$> quotedString
        <?> "chord symbol"



{- an annotation to the score
   4.19 Annotations

   General text annotations can be added above, below or on the staff in a similar way to chord symbols. In this case, the string within double quotes
   is preceded by one of five symbols ^, _, <, > or @ which controls where the annotation is to be placed; above, below, to the left or right respectively
   of the following note, rest or bar line. Using the @ symbol leaves the exact placing of the string to the discretion of the interpreting program.
   These placement specifiers distinguish annotations from chord symbols, and should prevent programs from attempting to play or transpose them.
   All text that follows the placement specifier is treated as a text string.

   Example:

   "<(" ">)" C

-}


annotation : Parser s Music
annotation =
    buildAnnotation
        <$> annotationString
        <?> "annotation"


chord : Parser s Music
chord =
    Chord
        <$> abcChord
        <?> "chord"


inline : Parser s Music
inline =
    Inline
        <$> between (char '[') (char ']') (tuneBodyInfo True)
        <?> "inline header"


graceNote : Parser s Music
graceNote =
    between (char '{') (char '}') grace
        <?> "grace note"


grace : Parser s Music
grace =
    GraceNote <$> acciaccatura <*> (many1 abcNote)



{- acciaccaturas are indicated with an optional forward slash
   was
    acciaccatura = withDefault False <$> ( (\_ -> True) <$> maybe (char '/'))
-}


acciaccatura : Parser s Bool
acciaccatura =
    (\_ -> True) <$> maybe (char '/')


decoration : Parser s Music
decoration =
    Decoration
        <$> choice [ shortDecoration, longDecoration ]
        <?> "decoration"


shortDecoration : Parser s String
shortDecoration =
    -- regex "[\\.~HLMOSTuv]"
    regex "[\\.~HLMOPSTuv]"
        <?> "short decoration"


longDecoration : Parser s String
longDecoration =
    between (char '!') (char '!') (regex "[^\x0D\n!]*")
        <?> "long decoration"



-- general attributes
-- e.g 3/4


rational : Parser s Rational
rational =
    Ratio.over <$> int <* char '/' <*> int



-- e.g. /4 (as found in note durations)


curtailedLeftRational : Parser s Rational
curtailedLeftRational =
    Ratio.over 1 <$> (char '/' *> int)



-- e.g. 3/ (as found in note durations)


curtailedRightRational : Parser s Rational
curtailedRightRational =
    invert <$> (Ratio.over 2 <$> (int <* char '/'))



{- e.g. / or // or /// (as found in note durations)
   which translates to 1/2, 1/4, 1/8 etc
-}


slashesRational : Parser s Rational
slashesRational =
    buildRationalFromExponential <$> (List.length <$> (many1 <| char '/'))



-- HEADER ATTRIBUTES
-- rational with trailing optional spaces


headerRational : Parser s Rational
headerRational =
    rational <* whiteSpace



{- experimental -}


meterDefinition : Parser s (Maybe MeterSignature)
meterDefinition =
    choice
        [ cutTime
        , commonTime
        , meterSignature
        , nometer
        ]


commonTime : Parser s (Maybe MeterSignature)
commonTime =
    (Just ( 4, 4 )) <$ char 'C'


cutTime : Parser s (Maybe MeterSignature)
cutTime =
    (Just ( 2, 2 )) <$ string "C|"



-- can't use Rationals for these because they cancel


meterSignature : Parser s (Maybe MeterSignature)
meterSignature =
    Just <$> ((,) <$> int <* char '/' <*> int <* whiteSpace)


nometer : Parser s (Maybe MeterSignature)
nometer =
    Nothing <$ string "none"


noteDuration : Parser s NoteDuration
noteDuration =
    rational <* whiteSpace



{- This is an area where we've relaxed the 2.1 spec
   1) we use many rather than many1 note length designators
   2) equals is now optional
   This means we can parse all of
        1/4 3/8 1/4 3/8=40
        "Allegro" 1/4=120
        3/8=50 "Slowly"
   but also the outdated (but prevalent in the wild
   120  (which means 1/4=120)
-}


tempoSignature : Parser s TempoSignature
tempoSignature =
    buildTempoSignature <$> maybe spacedQuotedString <*> many headerRational <*> maybe (char '=') <*> int <*> maybe spacedQuotedString <* whiteSpace



-- accidental in a key signature (these use a different representation from accidentals in the tune body)


sharpOrFlat : Parser s Accidental
sharpOrFlat =
    map
        (\x ->
            if x == '#' then
                Sharp
            else
                Flat
        )
        (choice [ char '#', char 'b' ])


keyName : Parser s String
keyName =
    regex "[A-G]"


keySignature : Parser s KeySignature
keySignature =
    buildKeySignature <$> keyName <*> maybe sharpOrFlat <*> maybe mode



-- a complete list of key accidentals which may be empty


keyAccidentals : Parser s KeySet
keyAccidentals =
    buildKeyAccidentals <$> spacelessAccidental <*> keyAccidentalsList



-- I think the first in the list is optionally introduced without a space  (judging by what's in the wild)


spacelessAccidental : Parser s (Maybe KeyAccidental)
spacelessAccidental =
    maybe keyAccidental



-- there may be zero or more key accidentals, separated by spaces (KeySet is a List of Key Accidentals)


keyAccidentalsList : Parser s KeySet
keyAccidentalsList =
    many (space *> keyAccidental)



-- a key accidental as an amendment to a key signature - as in e.g. K:D Phr ^f


keyAccidental : Parser s KeyAccidental
keyAccidental =
    buildKeyAccidental <$> accidental <*> pitch


mode : Parser s Mode
mode =
    choice
        [ major
        , ionian
        , dorian
        , phrygian
        , lydian
        , mixolydian
        , aeolian
        , locrian
        , minor
          -- place last because of potential ambiguity
        ]


minor : Parser s Mode
minor =
    Minor <$ whiteSpace <* regex "(M|m)([A-Za-z])*"


major : Parser s Mode
major =
    Major <$ whiteSpace <* regex "(M|m)(A|a)(J|j)([A-Za-z])*"


ionian : Parser s Mode
ionian =
    Ionian <$ whiteSpace <* regex "(I|i)(O|o)(N|n)([A-Za-z])*"


dorian : Parser s Mode
dorian =
    Dorian <$ whiteSpace <* regex "(D|d)(O|o)(R|r)([A-Za-z])*"


phrygian : Parser s Mode
phrygian =
    Phrygian <$ whiteSpace <* regex "(P|p)(H|h)(R|r)([A-Za-z])*"


lydian : Parser s Mode
lydian =
    Lydian <$ whiteSpace <* regex "(L|l)(Y|y)(D|d)([A-Za-z])*"


mixolydian : Parser s Mode
mixolydian =
    Mixolydian <$ whiteSpace <* regex "(M|m)(I|i)(X|x)([A-Za-z])*"


aeolian : Parser s Mode
aeolian =
    Aeolian <$ whiteSpace <* regex "(A|a)(E|e)(O|o)([A-Za-z])*"


locrian : Parser s Mode
locrian =
    Locrian <$ whiteSpace <* regex "(L|l)(O|o)(C|c)([A-Za-z])*"



-- Headers


area : Parser s Header
area =
    Area
        <$> ((headerCode 'A') *> strToEol)
        <?> "A header"


book : Parser s Header
book =
    Book
        <$> ((headerCode 'B') *> strToEol)
        <?> "B Header"


composer : Parser s Header
composer =
    Composer
        <$> ((headerCode 'C') *> strToEol)
        <?> "C header"


discography : Parser s Header
discography =
    Discography
        <$> ((headerCode 'D') *> strToEol)
        <?> "D header"


fileUrl : Parser s Header
fileUrl =
    FileUrl
        <$> ((headerCode 'F') *> strToEol)
        <?> "F header"


group : Parser s Header
group =
    Group
        <$> ((headerCode 'G') *> strToEol)
        <?> "G header"


history : Parser s Header
history =
    History
        <$> ((headerCode 'H') *> strToEol)
        <?> "H header"


instruction : Bool -> Parser s Header
instruction isInline =
    Instruction
        <$> ((headerCode 'I') *> (inlineInfo isInline))
        <?> "I header"


key : Parser s Header
key =
    buildKey
        <$> (headerCode 'K')
        <*> keySignature
        <*> keyAccidentals
        <* whiteSpace
        <?> "K header"


unitNoteLength : Parser s Header
unitNoteLength =
    UnitNoteLength
        <$> ((headerCode 'L') *> noteDuration)
        <?> "L header"


meter : Parser s Header
meter =
    Meter
        <$> ((headerCode 'M') *> meterDefinition)
        <?> "M header"


macro : Bool -> Parser s Header
macro isInline =
    Macro
        <$> ((headerCode 'm') *> (inlineInfo isInline))
        <?> "m header"


notes : Bool -> Parser s Header
notes isInline =
    Notes
        <$> ((headerCode 'N') *> (inlineInfo isInline))
        <?> "N header"


origin : Parser s Header
origin =
    Origin
        <$> ((headerCode 'O') *> strToEol)
        <?> "O header"


parts : Bool -> Parser s Header
parts isInline =
    Parts
        <$> ((headerCode 'P') *> (inlineInfo isInline))
        <?> "P header"


tempo : Parser s Header
tempo =
    Tempo
        <$> ((headerCode 'Q') *> tempoSignature)
        <?> "Q header"


rhythm : Bool -> Parser s Header
rhythm isInline =
    Rhythm
        <$> ((headerCode 'R') *> (inlineInfo isInline))
        <?> "R header"


remark : Bool -> Parser s Header
remark isInline =
    Remark
        <$> ((headerCode 'r') *> (inlineInfo isInline))
        <?> "r header"


source : Parser s Header
source =
    Source
        <$> ((headerCode 'S') *> strToEol)
        <?> "S header"


symbolLine : Bool -> Parser s Header
symbolLine isInline =
    SymbolLine
        <$> ((headerCode 's') *> (inlineInfo isInline))
        <?> "s header"


title : Bool -> Parser s Header
title isInline =
    Title
        <$> ((headerCode 'T') *> (inlineInfo isInline))
        <?> "T header"


userDefined : Bool -> Parser s Header
userDefined isInline =
    UserDefined
        <$> ((headerCode 'U') *> (inlineInfo isInline))
        <?> "U header"


voice : Bool -> Parser s Header
voice isInline =
    Voice
        <$> ((headerCode 'V') *> (inlineInfo isInline))
        <?> "V header"


wordsAfter : Bool -> Parser s Header
wordsAfter isInline =
    WordsAfter
        <$> ((headerCode 'W') *> (inlineInfo isInline))
        <?> "W header"


wordsAligned : Bool -> Parser s Header
wordsAligned isInline =
    WordsAligned
        <$> ((headerCode 'w') *> (inlineInfo isInline))
        <?> "w header"


referenceNumber : Parser s Header
referenceNumber =
    ReferenceNumber
        <$> ((headerCode 'X') *> int)
        <?> "x header"


transcription : Parser s Header
transcription =
    Transcription
        <$> ((headerCode 'Z') *> strToEol)
        <?> "Z header"


fieldContinuation : Parser s Header
fieldContinuation =
    FieldContinuation
        <$> ((headerCode '+') *> strToEol)
        <?> "field continuation"



{- a header is an information field up to and including the end of line marker -}


header : Parser s Header
header =
    informationField False <* eol



{- unsupported header reserved for future use -}


unsupportedHeader : Parser s Header
unsupportedHeader =
    UnsupportedHeader
        <$ unsupportedHeaderCode
        <* strToEol
        <?> "unsupported header"



{- ditto for headers that may appear in the tune body -}


tuneBodyHeader : Parser s BodyPart
tuneBodyHeader =
    BodyInfo
        <$> tuneBodyInfo True
        <* eol
        <?> "tune body header"



{- Headers/Information fields.  These can be used in three different ways:
     1) As a normal tune header
     2) As an 'inline' header inside the tune body on a separate line
     3) Embedded inside a tune score between '[' and ']'

   Only a named subset of headers can be used inline in this way.

   One subtlety is therefore that header information that accepts simple text content
   should not be allowed to incorporate '[' or ']' because of the potential ambiguity.
   Thus, headers functions are given a parameter 'inline' which is the inline context
   simply allowing 'normal' headers to accept these values in text content but to allow
   inline headers to reject them.
-}
{- whereas information fields can be used inline
   isInline - is this information field being used in an in-line context
   (as opposed to being used in a conventional header)
-}


informationField : Bool -> Parser s Header
informationField isInline =
    -- log "header" <$>
    (choice
        [ anywhereInfo isInline
        , tuneInfo
        ]
        <?> "header"
    )



{- these can only be used in 'normal' headers -}


tuneInfo : Parser s Header
tuneInfo =
    choice
        [ area
        , book
        , composer
        , discography
        , fileUrl
        , group
        , history
        , origin
        , source
        , referenceNumber
        , transcription
        , unsupportedHeader
          -- headers that are currently unsupported but must be recognized and ignored
        ]
        <?> "tune info"


anywhereInfo : Bool -> Parser s Header
anywhereInfo isInline =
    choice
        [ instruction isInline
        , key
        , unitNoteLength
        , meter
        , macro isInline
        , notes isInline
        , parts isInline
        , tempo
        , rhythm isInline
        , remark isInline
        , title isInline
        , userDefined isInline
        , voice isInline
        , wordsAfter isInline
        , fieldContinuation
        , comment
        ]
        <?> "anywhere info"


tuneBodyOnlyInfo : Bool -> Parser s Header
tuneBodyOnlyInfo isInline =
    choice
        [ symbolLine isInline
        , wordsAligned isInline
        ]
        <?> "tune body only info"


tuneBodyInfo : Bool -> Parser s Header
tuneBodyInfo isInline =
    choice
        [ tuneBodyOnlyInfo isInline
        , anywhereInfo isInline
        ]
        <?> "tune body info"



{- relax the spec in the parsing of headers to allow body-only tunes -}


headers : Parser s TuneHeaders
headers =
    many header <?> "headers"



-- headers = many1 header <?> "headers"
{- comments.  These are introduced with '%' and can occur anywhere.
   The stylesheet directive '%%' is not recognized here and will
   simply be treated as a comment.  We'll treat comments as Headers
   so as not to pollute the parse tree overmuch
-}


comment : Parser s Header
comment =
    Comment
        <$> (regex "%" *> strToEol)
        <?> "comment"



-- low level parsers
-- possible whitespace


whiteSpace : Parser s String
whiteSpace =
    String.fromList <$> (many <| choice [ space, tab ])



-- at least one (intended) space somewhere inside the music body


spacer : Parser s Music
spacer =
    Spacer
        <$> (List.length <$> (many1 scoreSpace))
        <?> "space"



{- space within a line of the tune's score -}


scoreSpace : Parser s Char
scoreSpace =
    choice
        [ space
        , char 'y'
        , tab
        ]



{- characters to ignore

   Section 8.1 Tune Body:

   The following characters are currently reserved: # * ; ? @
   In future standards they may be used to extend the abc syntax. To ensure forward compatibility,
   current software should ignore these characters when they appear inside or between note groups.

   section 4.7 Beams:

   Back quotes ` may be used freely between notes to be beamed, to increase legibility.
   They are ignored by computer programs. For example, A2``B``C is equivalent to A2BC.
-}


ignore : Parser s Music
ignore =
    succeed Ignore
        <* (regex "[#@;`\\*\\?]+")
        <?> "ignored character"



{- this is an area where the spec is uncertain.  See 6.1.1 Typesetting line-breaks
   The forward slash is used to indicate 'continuation of input lines' often because
   users may need to avoid long lines if, for example, they would otherwise extend
   beyond the limit of an old email system.  All very out of date, but nevertheless
   still prevalent in the wild.  We take the view that we must do our best to recognise
   them and then throw them away (along with any other later stuff in the line)

   Return Continuation if we have a continuation
-}


continuation : Parser s Music
continuation =
    succeed Continuation
        <* char '\\'
        <* regex "[^\x0D\n]*"
        <?> "continuation"


headerCode : Char -> Parser s Char
headerCode c =
    char c <* char ':' <* whiteSpace


unsupportedHeaderCode : Parser s String
unsupportedHeaderCode =
    regex "[a-qt-vx-zEJ]" <* char ':' <* whiteSpace


spacedQuotedString : Parser s String
spacedQuotedString =
    whiteSpace *> quotedString <* whiteSpace


quotedString : Parser s String
quotedString =
    string "\""
        *> regex "(\\\\\"|[^\"\n])*"
        <* string "\""
        <?> "quoted string"


annotationString : Parser s String
annotationString =
    string "\""
        *> regex "[\\^\\>\\<-@](\\\\\"|[^\"\n])*"
        <* string "\""
        <?> "annotation"



{- parse a remaining string up to but not including the end of line
   was
      strToEol = String.fromList <$> many (noneOf [ '\r', '\n' ])
-}


strToEol : Parser s String
strToEol =
    regex "[^\x0D\n]*"



{- parse an information item String - note that, because these can be used inline
   (bracketed by '[' and ']') it behoves us not to use the framing characters in the string
   when the header is used inline (but not when used in a normal header)
   not that the spec has anything to say about it as far as I can see
-}


inlineInfo : Bool -> Parser s String
inlineInfo isInline =
    let
        pattern =
            if isInline then
                "[^\x0D\n\\[\\]]*"
            else
                "[^\x0D\n]*"
    in
        regex pattern


note : Parser s Music
note =
    Note <$> abcNote


abcNote : Parser s AbcNote
abcNote =
    buildNote
        <$> maybeAccidental
        <*> pitch
        <*> moveOctave
        <*> maybe noteDur
        <*> maybeTie
        <?> "ABC note"


abcChord : Parser s AbcChord
abcChord =
    buildChord
        <$> (between (char '[') (char ']') (many1 abcNote))
        <*> maybe noteDur
        <?> "ABC chord"



{- an upper or lower case note ([A-Ga-g]) -}


pitch : Parser s String
pitch =
    regex "[A-Ga-g]"



-- maybe an accidental defining a note's pitch


maybeAccidental : Parser s (Maybe Accidental)
maybeAccidental =
    maybe accidental


accidental : Parser s Accidental
accidental =
    buildAccidental
        <$> (choice
                [ string "^^"
                , string "__"
                , string "^"
                , string "_"
                , string "="
                ]
            )



{- move an octave up (+ve - according to the number of apostrophes parsed)
   or down (-ve - according to the number of commas parsed)
-}


moveOctave : Parser s Int
moveOctave =
    octaveShift <$> regex "[',]*"



{- count the number of apostrophe (up) or comma (down) characters in the string
   and give the result a value of (up-down)
-}


octaveShift : String -> Int
octaveShift s =
    let
        f c acc =
            case c of
                '\'' ->
                    let
                        ( up, down ) =
                            acc
                    in
                        ( up + 1, down )

                ',' ->
                    let
                        ( up, down ) =
                            acc
                    in
                        ( up, down + 1 )

                _ ->
                    acc

        octs =
            String.foldl f ( 0, 0 ) s
    in
        (first octs - second octs)



{- the duration of a note in the body
   order of choices here is important to remove ambiguity
-}


noteDur : Parser s Rational
noteDur =
    choice
        [ rational
          -- e.g. 3/2
        , curtailedRightRational
          -- e.g. 3/
        , integralAsRational
          -- e.g. 3
        , curtailedLeftRational
          -- e.g. /2
        , slashesRational
          -- e.g. / or //
        ]



{- now attached to leading note and not free-standing -}


maybeTie : Parser s (Maybe Char)
maybeTie =
    (maybe (char '-'))
        <?> "tie"


integralAsRational : Parser s Rational
integralAsRational =
    Ratio.fromInt <$> int


tuplet : Parser s Music
tuplet =
    Tuplet
        <$> (char '(' *> tupletSignature)
        <*> many1 abcNote
        <?> "tuplet"



{- possible tuplet signatures
   (3             --> {3,2,3}
   (3:2           --> {3,2,3}
   (3::           --> {3,2,3}
   (3:2:4         --> {3,2,4}
   (3::2          --> {3,2,2}

   note, space is allowed after the tuplet signature but before the notes in the tuplet
-}


tupletSignature : Parser s TupletSignature
tupletSignature =
    buildTupletSignature
        <$> regex "[2-9]"
        <*> tup
        <*> tup
        <* whiteSpace


tup : Parser s (Maybe String)
tup =
    join
        <$> maybe
                (char ':' *> maybe (regex "[2-9]"))



-- builders
{- invert a Rational -}


invert : Rational -> Rational
invert r =
    let
        unit =
            fromInt 1
    in
        divide unit r



{- used in counting slashes exponentially -}


buildRationalFromExponential : Int -> Rational
buildRationalFromExponential i =
    Ratio.over 1 (2 ^ i)



-- build a tempo signature


buildTempoSignature : Maybe String -> List Rational -> Maybe Char -> Int -> Maybe String -> TempoSignature
buildTempoSignature ms1 fs c i ms2 =
    let
        ms =
            case ms1 of
                Nothing ->
                    ms2

                _ ->
                    ms1

        noteLengths =
            if (List.isEmpty fs) then
                -- cover the degenerate case of 'Q: 120' which means 'Q: 1/4=120'
                [ Ratio.over 1 4 ]
            else
                fs
    in
        { noteLengths = noteLengths
        , bpm = i
        , marking = ms
        }



{- Dictionary for PitchClass -}


pitchClassDict : Dict String PitchClass
pitchClassDict =
    Dict.fromList
        [ ( "A", A )
        , ( "B", B )
        , ( "C", C )
        , ( "D", D )
        , ( "E", E )
        , ( "F", F )
        , ( "G", G )
        ]


lookupPitch : String -> PitchClass
lookupPitch p =
    Dict.get (String.toUpper p) pitchClassDict
        |> withDefault C



-- build a key signature


buildKeySignature : String -> Maybe Accidental -> Maybe Mode -> KeySignature
buildKeySignature pStr ma mm =
    { pitchClass = lookupPitch pStr, accidental = ma, mode = withDefault Major mm }



-- build a complete key designation (key signature plus modifying accidentals)


buildKey : Char -> KeySignature -> List KeyAccidental -> Header
buildKey c ks ka =
    Key ( ks, ka )



{- build a bar line
   this is a bit tricky because of the poor specification for the possible shapes of bar lines
   which may have multiple different types of bar line markers (|,[,]) and repeat markers (:)
   Try to normalise to representations of basic shapes like (|, |:, :|, :||, ||:, ||, :|:, :||: )

-}


buildBarline : String -> Maybe Int -> Music
buildBarline s i =
    let
        -- estimate the bar separator thickness
        thickness =
            if (String.contains "|]" s) then
                ThinThick
            else if (String.contains "[|" s) then
                ThickThin
            else if (String.contains "||" s) then
                ThinThin
            else
                Thin

        -- now normalise all lines to '|'
        f c =
            case c of
                '[' ->
                    '|'

                ']' ->
                    '|'

                _ ->
                    c

        normalised =
            String.map f s

        -- count the repeat markers
        repeatCount =
            String.length (String.filter (\c -> c == ':') normalised)

        -- set the repeat
        repeat =
            if (repeatCount == 0) then
                Nothing
            else if (repeatCount == 1) then
                if String.contains ":|" normalised then
                    Just End
                else
                    Just Begin
            else
                Just BeginAndEnd
    in
        Barline { thickness = thickness, repeat = repeat, iteration = i }


buildNote : Maybe Accidental -> String -> Int -> Maybe Rational -> Maybe Char -> AbcNote
buildNote macc pitchStr octave ml mt =
    let
        l =
            withDefault (Ratio.fromInt 1) ml

        -- a = buildAccidental macc
        p =
            lookupPitch (String.toUpper pitchStr)

        spn =
            scientificPitchNotation pitchStr octave

        tied =
            case mt of
                Just _ ->
                    True

                _ ->
                    False
    in
        { pitchClass = p, accidental = macc, octave = spn, duration = l, tied = tied }



{- investigate a note/octave pair and return the octave
   in scientific pitch notation relative to MIDI pitches
-}


scientificPitchNotation : String -> Int -> Int
scientificPitchNotation pc oct =
    if (contains (Regex.regex "[A-G]")) pc then
        -- pitch class inhabits octave of middle C, oct <= 0
        middlecOctave + oct
    else
        -- pitch class inhabits octave above middle C, oct >= 0
        middlecOctave + 1 + oct


buildAccidental : String -> Accidental
buildAccidental s =
    case s of
        "^^" ->
            DoubleSharp

        "__" ->
            DoubleFlat

        "^" ->
            Sharp

        "_" ->
            Flat

        _ ->
            Natural


buildKeyAccidental : Accidental -> String -> KeyAccidental
buildKeyAccidental a pitchStr =
    let
        pc =
            lookupPitch pitchStr
    in
        ( pc, a )


buildKeyAccidentals : Maybe KeyAccidental -> List KeyAccidental -> List KeyAccidental
buildKeyAccidentals mac acs =
    case mac of
        Just ac ->
            ac :: acs

        _ ->
            acs


buildChord : List AbcNote -> Maybe Rational -> AbcChord
buildChord ns ml =
    let
        l =
            withDefault (Ratio.fromInt 1) ml
    in
        { notes = ns, duration = l }



{- build a tuplet signature {p,q,r) - p notes in the time taken for q
   in operation over the next r notes
-}


buildTupletSignature : String -> Maybe String -> Maybe String -> TupletSignature
buildTupletSignature ps mq mr =
    let
        p =
            toTupletInt ps

        -- default values for q.  Not quite in accordance with spec where q varies
        -- between 2 and 3 for odd values of p, dependent on the time signature
        -- (but this would make the parser stateful which we don't want for such small
        -- edge cases)
        qdefault =
            case p of
                2 ->
                    3

                3 ->
                    2

                4 ->
                    3

                6 ->
                    2

                8 ->
                    3

                _ ->
                    2

        q =
            withDefault qdefault (Maybe.map toTupletInt mq)

        r =
            withDefault p (Maybe.map toTupletInt mr)
    in
        ( p, q, r )


buildBrokenOperator : String -> Broken
buildBrokenOperator s =
    if String.startsWith "<" s then
        LeftArrow (String.length s)
    else
        RightArrow (String.length s)


buildAnnotation : String -> Music
buildAnnotation s =
    let
        firstChar =
            List.head (String.toList s)

        placement =
            case firstChar of
                Just '^' ->
                    AboveNextSymbol

                Just '_' ->
                    BelowNextSymbol

                Just '<' ->
                    LeftOfNextSymbol

                Just '>' ->
                    RightOfNextSymbol

                _ ->
                    Discretional
    in
        Annotation placement s


toTupletInt : String -> Int
toTupletInt s =
    s
        |> String.toInt
        |> Result.toMaybe
        |> withDefault 3



-- default can't happen because all strings are regex-parsed 2-9
{- just for debug purposes - consume the rest of the input -}
{-
   restOfInput : Parser (List Char)
   restOfInput = many anyChar
-}
-- exported functions


{-| Entry point - Parse an ABC tune image.
-}
parse : String -> Result.Result ParseError AbcTune
parse s =
    case Combine.parse abc s of
        Ok ( _, _, n ) ->
            Ok n

        Err ( _, ctx, msgs ) ->
            Err { msgs = msgs, input = ctx.input, position = ctx.position }


{-| Parse a key signature.
    A utility function for applications needing to parse key signatures in isolation
    and returning them as a ModifiedKeySignature (where the modification is empty).
-}
parseKeySignature : String -> Result.Result ParseError ModifiedKeySignature
parseKeySignature s =
    case Combine.parse keySignature s of
        Ok ( _, _, n ) ->
            Ok ( n, [] )

        Err ( _, ctx, msgs ) ->
            Err { msgs = msgs, input = ctx.input, position = ctx.position }


{-| Format a parse error as a string.
-}
parseError : ParseError -> String
parseError pe =
    let
        append a b =
            a ++ "," ++ b

        msg =
            List.foldr append "" pe.msgs
    in
        "parse error: " ++ msg ++ " on " ++ pe.input ++ " at position " ++ toString (pe.position)
