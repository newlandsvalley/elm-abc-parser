module Test.Abc exposing (tests, singletest)

import Test exposing (..)
import Expect exposing (..)
import Abc exposing (parse, parseKeySignature, parseError)
import Abc.ParseTree exposing (PitchClass(..), KeySignature, Accidental(..), Mode(..), AbcNote)
import Abc.Canonical exposing (fromTune)
import String


{- round trip ABC -> ParseTree -> ABC -}


roundTrip : String -> String
roundTrip s =
    let
        parseResult =
            parse s
    in
        case parseResult of
            Ok res ->
                fromTune res

            Err errs ->
                "Fail: " ++ (parseError errs)



{- assert round-tripping works -}


assertRoundTrip : String -> Expectation
assertRoundTrip s =
    Expect.equal s (roundTrip s)



{- assert the input parses -}


assertParses : String -> Expectation
assertParses s =
    let
        parseResult =
            parse s
    in
        case parseResult of
            Ok res ->
                Expect.pass

            Err errs ->
                Expect.fail "parse error"



{- assert the key signature parses -}


assertKeySigParses : String -> Expectation
assertKeySigParses s =
    let
        parseResult =
            parseKeySignature s
    in
        case parseResult of
            Ok res ->
                Expect.pass

            Err errs ->
                Expect.fail "bad key signature"



{- assert the input doesn't parse -}


assertParseError : String -> Expectation
assertParseError s =
    let
        parseResult =
            parse s
    in
        case parseResult of
            Ok res ->
                Expect.fail "parses when it shouldn't"

            Err err ->
                Expect.pass



{- assert the input parses and the canonicalised tune equals the target -}


assertCanonicalMatches : String -> String -> Expectation
assertCanonicalMatches s target =
    let
        parseResult =
            parse s
    in
        case parseResult of
            Ok res ->
                Expect.equal target (fromTune res)

            Err errs ->
                Expect.fail "canonical mismatch"



{- assert the input doesn't parse and gives the correct error position -}


assertErrorPos : String -> Int -> Expectation
assertErrorPos s pos =
    let
        parseResult =
            parse s
    in
        case parseResult of
            Ok res ->
                Expect.fail "parses when it shouldn't"

            Err err ->
                Expect.equal pos err.position


tests : Test
tests =
    let
        header =
            describe "Octave Change"
                [ test "area" <|
                    \() -> (assertRoundTrip area)
                , test "book" <|
                    \() -> (assertRoundTrip book)
                , test "composer" <|
                    \() -> (assertRoundTrip composer)
                , test "discography" <|
                    \() -> (assertRoundTrip discography)
                , test "file URL" <|
                    \() -> (assertRoundTrip fileurl)
                , test "group" <|
                    \() -> (assertRoundTrip group)
                , test "history" <|
                    \() -> (assertRoundTrip history)
                , test "instruction" <|
                    \() -> (assertRoundTrip instruction)
                , test "key" <|
                    \() -> (assertRoundTrip key)
                , test "key with accidental" <|
                    \() -> (assertRoundTrip keyWithAccidental)
                , test "key with unspaced accidental" <|
                    \() -> (assertParses keyWithUnspacedAccidental)
                , test "unit note length" <|
                    \() -> (assertRoundTrip noteLength)
                , test "meter" <|
                    \() -> (assertRoundTrip meter)
                , test "no meter" <|
                    \() -> (assertRoundTrip nometer)
                , test "macro" <|
                    \() -> (assertRoundTrip macro)
                , test "notes" <|
                    \() -> (assertRoundTrip notes)
                , test "origin" <|
                    \() -> (assertRoundTrip origin)
                , test "parts" <|
                    \() -> (assertRoundTrip parts)
                , test "tempo" <|
                    \() -> (assertRoundTrip tempo)
                , test "tempo with space" <|
                    \() -> (assertParses tempoSpace)
                , test "complex tempo" <|
                    \() -> (assertRoundTrip tempoComplex)
                , test "tempo no note length" <|
                    \() -> (assertCanonicalMatches tempoNoNoteLength tempoNoNoteLengthCanonical)
                , test "remark" <|
                    \() -> (assertRoundTrip remark)
                , test "rhythm" <|
                    \() -> (assertRoundTrip rhythm)
                , test "source" <|
                    \() -> (assertRoundTrip source)
                , test "title" <|
                    \() -> (assertRoundTrip title)
                , test "user-defined" <|
                    \() -> (assertRoundTrip userDefined)
                , test "voice" <|
                    \() -> (assertRoundTrip voice)
                , test "words after" <|
                    \() -> (assertRoundTrip wordsAfter)
                , test "words aligned" <|
                    \() -> (assertRoundTrip wordsAligned)
                , test "reference number" <|
                    \() -> (assertRoundTrip reference)
                , test "transcriber" <|
                    \() -> (assertRoundTrip transcriber)
                , test "field continuation" <|
                    \() -> (assertRoundTrip fieldContinuation)
                , test "comment" <|
                    \() -> (assertRoundTrip comment)
                , test "unsupported header" <|
                    \() -> (assertParses unsupportedHeader)
                , test "bracket in header" <|
                    \() -> (assertRoundTrip bracketInHeader)
                ]

        tune =
            describe "tune"
                [ test "note" <|
                    \() -> (assertRoundTrip note)
                , test "long note" <|
                    \() -> (assertRoundTrip longNote)
                , test "fractionalNote" <|
                    \() -> (assertParses fractionalNote)
                , test "left fractionalNote" <|
                    \() -> (assertParses leftFractionalNote)
                , test "right fractionalNote" <|
                    \() -> (assertParses rightFractionalNote)
                , test "double fractionalNote" <|
                    \() -> (assertParses doubleFractionalNote)
                , test "broken rhythm" <|
                    \() -> (assertRoundTrip brokenRhythm)
                , test "broken rhythm spaced" <|
                    \() -> (assertCanonicalMatches brokenRhythmSpaced brokenRhythmSpacedCanonical)
                , test "accidentals" <|
                    \() -> (assertRoundTrip accidentals)
                , test "octave" <|
                    \() -> (assertRoundTrip octave)
                , test "tie" <|
                    \() -> (assertRoundTrip tie)
                , test "complex tie" <|
                    \() -> (assertRoundTrip complextie)
                , test "barline" <|
                    \() -> (assertRoundTrip barline)
                , test "repeat" <|
                    \() -> (assertRoundTrip repeat)
                , test "repeat with [" <|
                    \() -> (assertCanonicalMatches repeat1 repeat)
                , test "repeat with spaced [" <|
                    \() -> (assertCanonicalMatches repeat2 repeat)
                , test "degenerateRepeat" <|
                    \() -> (assertCanonicalMatches degenerateRepeat degenerateRepeatCanonical)
                , test "triplet" <|
                    \() -> (assertRoundTrip triplet)
                , test "tripletSpaced" <|
                    \() -> (assertCanonicalMatches tripletSpaced tripletSpacedCanonical)
                , test "tuplet" <|
                    \() -> (assertRoundTrip triplet)
                , test "slur" <|
                    \() -> (assertRoundTrip slur)
                , test "grace notes" <|
                    \() -> (assertRoundTrip grace)
                , test "chord symbols" <|
                    \() -> (assertRoundTrip chordSymbols)
                , test "chords" <|
                    \() -> (assertRoundTrip chords)
                , test "chord duration" <|
                    \() -> (assertRoundTrip chordDuration)
                , test "articulation" <|
                    \() -> (assertRoundTrip articulation)
                , test "ignore" <|
                    \() -> (assertParses ignore)
                , test "typesetSpace" <|
                    \() -> (assertParses typesetSpace)
                , test "backtick" <|
                    \() -> (assertParses backtick)
                , test "annotation" <|
                    \() -> (assertParses annotation)
                ]

        structure =
            describe "structure"
                [ test "inline" <|
                    \() -> (assertRoundTrip inline)
                , test "inlineBracket" <|
                    \() -> (assertRoundTrip inlineBracket)
                , test "inlineBracket1" <|
                    \() -> (assertRoundTrip inlineBracket1)
                , test "inlineKey" <|
                    \() -> (assertRoundTrip inlineKey)
                , test "inlineComment" <|
                    \() -> (assertRoundTrip inlineComment)
                , test "new tempo" <|
                    \() -> (assertRoundTrip newTempo)
                , test "new key" <|
                    \() -> (assertRoundTrip newKey)
                , test "new unit length" <|
                    \() -> (assertRoundTrip newUnitLength)
                , test "new part" <|
                    \() -> (assertRoundTrip newPart)
                ]

        badInput =
            describe "bad input"
                [ test "bad characters 1" <|
                    \() -> (assertErrorPos badChars1 22)
                , test "bad characters 2" <|
                    \() -> (assertErrorPos badChars2 3)
                , test "bracket in inline header" <|
                    \() -> (assertErrorPos bracketInInlineHeader 9)
                ]

        canonical =
            describe "canonical"
                [ test "implied accidentals in key" <|
                    \() -> (assertRoundTrip canonicalised)
                  -- probably dropping this from Canonical now
                  -- , test "explicit accidentals" (assertCanonicalMatches uncanonicalised canonicalised)
                ]

        keySignature =
            describe "key signature"
                [ test "C# Major" <|
                    \() -> (assertKeySigParses "C# Major")
                , test "Bb Minor" <|
                    \() -> (assertKeySigParses "Bb Minor")
                , test "G Mixolydian" <|
                    \() -> (assertKeySigParses "G Mixolydian")
                , test "A Locrian" <|
                    \() -> (assertKeySigParses "A Locrian")
                ]

        single =
            describe "single test"
                [ test "inlineKey" <|
                    \() -> (assertRoundTrip inlineKey)
                ]
    in
        concat
            [ header
            , tune
            , structure
            , badInput
            , canonical
            , keySignature
            ]



-- a simple wrapper for running a single test


singletest : Test
singletest =
    describe "single test"
        [ test "inlineKey" <|
            \() -> (assertRoundTrip inlineKey)
        ]



-- these ABC samples must already be in canonical format for round-tripping to work
-- because of the exact string matching algorithm
-- music


note =
    "| ABC z2 def z/ |\x0D\n"


longNote =
    "| ABC d12 |\x0D\n"


fractionalNote =
    "| A/2 B3/2 c/ |\x0D\n"


leftFractionalNote =
    "| A3/ |\x0D\n"


rightFractionalNote =
    "| A/3 |\x0D\n"


doubleFractionalNote =
    "| A// |\x0D\n"


brokenRhythm =
    "| A>B C>>D a<b c<<d |\x0D\n"


brokenRhythmSpaced =
    "| A> B |\x0D\n"


brokenRhythmSpacedCanonical =
    "| A>B |\x0D\n"


accidentals =
    "| ^A_B c=d_e |\x0D\n"


octave =
    "| A,B,,C z2 d'e''f z/ |\x0D\n"


tie =
    "| A4~ A2 |\x0D\n"


complextie =
    "| fg-ga ab-bc|\x0D\n"


barline =
    "[| ABC | def |]\x0D\n"


repeat =
    "|: ABCD EFGa |1 D4 C4 :|2 c8 |\x0D\n"


repeat1 =
    "|: ABCD EFGa |[1 D4 C4 :|[2 c8 |\x0D\n"


repeat2 =
    "|: ABCD EFGa | [1 D4 C4 :| [2 c8 |\x0D\n"


degenerateRepeat =
    "[1 ABCD |\x0D\n"


degenerateRepeatCanonical =
    "|1 ABCD |\x0D\n"


triplet =
    "| (3de^f (3cda |\x0D\n"


tuplet =
    "| (3:2:4d2e2^fg |\x0D\n"


tripletSpaced =
    "| (3 abc def |\x0D\n"


tripletSpacedCanonical =
    "| (3abc def |\x0D\n"


slur =
    "| (de^f) (cda) |\x0D\n"


grace =
    "| {d^f}GA |\x0D\n"


chordSymbols =
    "| \"Em\" EG \"Am\" AC |\x0D\n"


chords =
    "| [de^f]g [cda]b |\x0D\n"


chordDuration =
    "| [cda]4 |\x0D\n"


articulation =
    "(vA2 | !fz!Ld2).d.f .e.d.c.B A2(A2 | d2).d.f .e.d.c.B A2A2 |\x0D\n"


ignore =
    "| ABC# z2 @def z/ |\x0D\n"


typesetSpace =
    "| ABC yz2 defyz/ |\x0D\n"


backtick =
    "| A``B``C |\x0D\n"


annotation =
    "| \"<(\" \">)\" EG   |\x0D\n"



-- headers


area =
    "A: London\x0D\n| ABC |\x0D\n"


book =
    "B: Richie Robinson\x0D\n| ABC |\x0D\n"


composer =
    "C: Bys-Kalle\x0D\n| ABC |\x0D\n"


discography =
    "D: 2 Brudetstykke\x0D\n| ABC |\x0D\n"


fileurl =
    "F: http\\\\tradtunedb.org.uk\x0D\n| ABC |\x0D\n"


group =
    "G: Swåp\x0D\n| ABC |\x0D\n"


history =
    "H: Learned from AnnbjØrg Lien\x0D\n| ABC |\x0D\n"


instruction =
    "I: abc-charset UTF-8\x0D\n| ABC |\x0D\n"


key =
    "K: AMinor\x0D\n| ABC |\x0D\n"


keyWithAccidental =
    "K: AMinor ^f\x0D\n| ABC |\x0D\n"


keyWithUnspacedAccidental =
    "K: EMinor^c\x0D\n| ABC |\x0D\n"


noteLength =
    "L: 1/8\x0D\n| ABC |\x0D\n"


meter =
    "M: 3/4\x0D\n| ABC |\x0D\n"


nometer =
    "M: none\x0D\n| ABC |\x0D\n"


macro =
    "m: ~g2 = {a}g{f}g\x0D\n| ABC |\x0D\n"


notes =
    "N: from recording made at Tideswell\x0D\n| ABC |\x0D\n"


origin =
    "O: Skåne\x0D\n| ABC |\x0D\n"


parts =
    "P: ((AB)3.(CD)3)2\x0D\n| ABC |\x0D\n"


tempo =
    "Q: 1/4=120\x0D\n| ABC |\x0D\n"


tempoNoNoteLength =
    "Q: 70\x0D\n| ABC |\x0D\n"



-- this degenerate form...


tempoNoNoteLengthCanonical =
    "Q: 1/4=70\x0D\n| ABC |\x0D\n"



-- should expand to this in canonical


tempoSpace =
    "Q: 1/8=80 \x0D\n| ABC |\x0D\n"


tempoComplex =
    "Q: 1/4 3/8 1/4 3/8=40 \"allegro\"\x0D\n| ABC |\x0D\n"


remark =
    "r: this is a remark\x0D\n| ABC |\x0D\n"


rhythm =
    "R: Polska\x0D\n| ABC |\x0D\n"


source =
    "S: Christine Dyer\x0D\n| ABC |\x0D\n"


title =
    "T: Engelska efter Albert Augustsson\x0D\n| ABC |\x0D\n"


userDefined =
    "U: some comment\x0D\n| ABC |\x0D\n"


voice =
    "V: T1           clef=treble-8  name=\"Tenore I\"   snm=\"T.I\"\x0D\n| ABC |\x0D\n"


wordsAfter =
    "W: doh re mi fa \x0D\n| ABC |\x0D\n"


wordsAligned =
    "| ABC |\x0D\nw: doh re mi fa \x0D\n| ABC |\x0D\n"



-- only appears inline


reference =
    "X: 125\x0D\n| ABC |\x0D\n"


transcriber =
    "Z: John Watson\x0D\n| ABC |\x0D\n"


fieldContinuation =
    "R: Polska\x0D\n+: in triplet time\x0D\n| ABC |\x0D\n"


comment =
    "%%TBL:{\"version\":\"beta\",\"type\":\"tune\",\"id\":\"10294\"}\x0D\n| ABC |\x0D\n"


unsupportedHeader =
    "j: custom header\x0D\n| ABC |\x0D\n"


bracketInHeader =
    "r: this is a remark [part 1]\x0D\n| ABC |\x0D\n"



-- structure


inline =
    "| ABC z2 def z/ \x0D\nQ: 1/4=120\x0D\n| ABC z2 def z/ |\x0D\n"


inlineBracket =
    "| ABC def g3 | [L: 1/8] A3 A3 |\x0D\n"


inlineBracket1 =
    "| ABC def g3 |[L: 1/8] A3 A3 |\x0D\n"


newKey =
    "| ABc |\x0D\nK: F#Major\x0D\n| def |\x0D\n"


newTempo =
    "| ABc |\x0D\nM: 3/4\x0D\n| def |\x0D\n"


newUnitLength =
    "| ABc |\x0D\nL: 1/16\x0D\n| def |\x0D\n"


newPart =
    "| ABc |\x0D\nP: B\x0D\n| def |\x0D\n"



-- support no gap between bar line and inline header


inlineKey =
    "| ABC def g3 | [K: AMajor] g3 a3 |\x0D\n"


inlineComment =
    "| ABC z2 def z/ \x0D\n%% this is a comment\x0D\n| ABC z2 def z/ |\x0D\n"



-- bad input


badChars1 =
    "| ABC z2 def z/ |\x0D\n| foo bar |\x0D\n"


badChars2 =
    "| foo bar |\x0D\n| ABC z2 def z/ |\x0D\n"


bracketInInlineHeader =
    "| ABC |\x0D\nr: this is a remark [part 1]\x0D\n"



-- canonical


canonicalised =
    "K: DMajor\x0D\n| ACF |\x0D\n"



-- C and F are implictly sharpened and should remain implicit in canonical


uncanonicalised =
    "K: DMajor\x0D\n| A^C^F |\x0D\n"



-- should be canonicalised to 'canonicalised'
