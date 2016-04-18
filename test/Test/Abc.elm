module Test.Abc (tests) where

import ElmTest exposing (..)
import Abc exposing (parse, parseKeySignature, parseError)
import Abc.ParseTree exposing (PitchClass(..), KeySignature, Accidental(..), Mode(..), AbcNote)
import Abc.Canonical exposing (fromTune)


import String

{- round trip ABC -> ParseTree -> ABC -}
roundTrip : String -> String
roundTrip s =
  let 
    parseResult = parse s
  in 
    case parseResult of
      Ok res -> 
        fromTune res
      Err errs -> 
        "Fail: " ++ (parseError errs)

{- assert round-tripping works -}
assertRoundTrip : String -> Assertion
assertRoundTrip s =
  assertEqual s (roundTrip s)

{- assert the input parses -}
assertParses : String -> Assertion
assertParses s = 
  let 
    parseResult = parse s
  in 
    case parseResult of
      Ok res -> 
        assert True
      Err errs -> 
        assert False

{- assert the key signature parses -}
assertKeySigParses : String -> Assertion
assertKeySigParses s = 
  let 
    parseResult = parseKeySignature s
  in 
    case parseResult of
      Ok res -> 
        assert True
      Err errs -> 
        assert False

{- assert the input doesn't parse -}
assertParseError : String -> Assertion
assertParseError s =
  let 
    parseResult = parse s
  in 
    case parseResult of
      Ok res -> 
        assert False
      Err err -> 
        assert True

{- assert the input parses and the canonicalised tune equals the target -}
assertCanonicalMatches : String -> String -> Assertion
assertCanonicalMatches s target = 
  let 
    parseResult = parse s
  in 
    case parseResult of
      Ok res -> 
        assertEqual target (fromTune res)
      Err errs -> 
        assert False

{- assert the input doesn't parse and gives the correct error position -}
assertErrorPos : String -> Int -> Assertion
assertErrorPos s pos =
  let 
    parseResult = parse s
  in 
    case parseResult of
      Ok res -> 
        assert False
      Err err -> 
        assertEqual pos err.position 

tests : Test
tests =
  let 
    header = 
      suite "header"
        [ test "area" (assertRoundTrip area)
        , test "book" (assertRoundTrip book)
        , test "composer" (assertRoundTrip composer)
        , test "discography" (assertRoundTrip discography)
        , test "file URL" (assertRoundTrip fileurl)
        , test "group" (assertRoundTrip group)
        , test "history" (assertRoundTrip history)
        , test "instruction" (assertRoundTrip instruction)
        , test "key" (assertRoundTrip key)
        , test "key with accidental" (assertRoundTrip keyWithAccidental)
        , test "unit note length" (assertRoundTrip noteLength)
        , test "meter" (assertRoundTrip meter)
        , test "no meter" (assertRoundTrip nometer)
        , test "macro" (assertRoundTrip macro)
        , test "notes" (assertRoundTrip notes)
        , test "origin" (assertRoundTrip origin)
        , test "parts" (assertRoundTrip parts)
        , test "tempo" (assertRoundTrip tempo)
        , test "complex tempo" (assertRoundTrip tempoComplex)
        , test "remark" (assertRoundTrip remark)
        , test "rhythm" (assertRoundTrip rhythm)
        , test "source" (assertRoundTrip source)
        , test "title" (assertRoundTrip title)
        , test "user-defined" (assertRoundTrip userDefined)
        , test "voice" (assertRoundTrip voice)
        , test "words after" (assertRoundTrip wordsAfter)
        , test "words aligned" (assertRoundTrip wordsAligned)
        , test "reference number" (assertRoundTrip reference)
        , test "transcriber" (assertRoundTrip transcriber)
        , test "field continuation" (assertRoundTrip fieldContinuation)
        , test "comment" (assertRoundTrip comment)
        , test "unsupported header" (assertParses unsupportedHeader)
        , test "bracket in header" (assertRoundTrip bracketInHeader)
        ]
    tune =
      suite "tune"
        [ test "note" (assertRoundTrip note)
        , test "fractionalNote" (assertParses fractionalNote)
        , test "left fractionalNote" (assertParses leftFractionalNote)
        , test "right fractionalNote" (assertParses rightFractionalNote)
        , test "double fractionalNote" (assertParses doubleFractionalNote)
        , test "broken rhythm" (assertRoundTrip brokenRhythm)
        , test "accidentals" (assertRoundTrip accidentals)
        , test "octave" (assertRoundTrip octave)
        , test "tie" (assertRoundTrip tie)
        , test "complex tie" (assertRoundTrip complextie)
        , test "repeat" (assertRoundTrip repeat)
        , test "triplet" (assertRoundTrip triplet)
        , test "tuplet" (assertRoundTrip triplet)
        , test "slur" (assertRoundTrip slur)
        , test "grace notes" (assertRoundTrip grace)
        , test "chord symbols" (assertRoundTrip chordSymbols)
        , test "chords" (assertRoundTrip chords)
        , test "chord duration" (assertRoundTrip chordDuration)
        , test "articulation" (assertRoundTrip articulation)
        , test "ignore" (assertParses ignore)
        , test "typesetSpace" (assertParses typesetSpace)
        , test "backtick" (assertParses backtick)
        , test "annotation" (assertParses annotation)
        ]   
    structure = 
      suite "structure"
        [ test "inline" (assertRoundTrip inline)
        , test "inlineComment" (assertRoundTrip inlineComment)
        ]   
    badInput = 
      suite "bad input"
        [ test "bad characters 1" (assertErrorPos badChars1 22)
        , test "bad characters 2" (assertErrorPos badChars2 3)
        , test "bracket in inline header" (assertErrorPos bracketInInlineHeader 9)
        ]
    canonical = 
      suite "canonical"
        [ test "implied accidentals in key" (assertRoundTrip canonicalised)
        -- probably dropping this from Canonical now
        -- , test "explicit accidentals" (assertCanonicalMatches uncanonicalised canonicalised)
        ]
    keySignature =
      suite "key signature"
        [
          test "C# Major" (assertKeySigParses "C# Major")
        , test "Bb Minor" (assertKeySigParses "Bb Minor")
        , test "G Mixolydian" (assertKeySigParses "G Mixolydian")
        , test "A Locrian" (assertKeySigParses "A Locrian")
        ]
  in
    suite "Music Notation"
      [  header
      ,  tune
      ,  structure
      ,  badInput
      ,  canonical
      ,  keySignature
      ]

-- these ABC samples must already be in canonical format for round-tripping to work
-- because of the exact string matching algorithm

-- music
note = "| ABC z2 def z/ |\r\n"
fractionalNote = "| A/2 B3/2 c/ |\r\n"
leftFractionalNote = "| A3/ |\r\n"
rightFractionalNote = "| A/3 |\r\n"
doubleFractionalNote = "| A// |\r\n"
brokenRhythm = "| A>B C>>D a<b c<<d |\r\n"
accidentals = "| ^A_B c=d_e |\r\n"
octave = "| A,B,,C z2 d'e''f z/ |\r\n"
tie = "| A4~ A2 |\r\n"
complextie = "| fg-ga ab-bc|\r\n"
repeat = "|: ABCD EFGa |1 D4 C4 :|2 c8 |\r\n"
triplet = "| (3de^f (3cda |\r\n"
tuplet = "| (3:2:4d2e2^fg |\r\n"
slur = "| (de^f) (cda) |\r\n"
grace = "| {d^f}GA |\r\n"
chordSymbols = "| \"Em\" EG \"Am\" AC |\r\n"
chords = "| [de^f]g [cda]b |\r\n"
chordDuration = "| [cda]4 |\r\n"
articulation = "(vA2 | !fz!Ld2).d.f .e.d.c.B A2(A2 | d2).d.f .e.d.c.B A2A2 |\r\n"
ignore = "| ABC# z2 @def z/ |\r\n"
typesetSpace = "| ABC yz2 defyz/ |\r\n"
backtick = "| A``B``C |\r\n"
annotation = "| \"<(\" \">)\" EG   |\r\n"

-- headers
area = "A: London\r\n| ABC |\r\n"
book = "B: Richie Robinson\r\n| ABC |\r\n"
composer = "C: Bys-Kalle\r\n| ABC |\r\n"
discography = "D: 2 Brudetstykke\r\n| ABC |\r\n"
fileurl = "F: http\\\\tradtunedb.org.uk\r\n| ABC |\r\n"
group = "G: Swåp\r\n| ABC |\r\n"
history = "H: Learned from AnnbjØrg Lien\r\n| ABC |\r\n"
instruction = "I: abc-charset UTF-8\r\n| ABC |\r\n"
key = "K: AMinor\r\n| ABC |\r\n"
keyWithAccidental = "K: AMinor ^f\r\n| ABC |\r\n"
noteLength = "L: 1/8\r\n| ABC |\r\n"
meter = "M: 3/4\r\n| ABC |\r\n"
nometer = "M: none\r\n| ABC |\r\n"
macro = "m: ~g2 = {a}g{f}g\r\n| ABC |\r\n"
notes = "N: from recording made at Tideswell\r\n| ABC |\r\n"
origin = "O: Skåne\r\n| ABC |\r\n"
parts = "P: ((AB)3.(CD)3)2\r\n| ABC |\r\n"
tempo = "Q: 1/4=120\r\n| ABC |\r\n"
tempoComplex = "Q: 1/4 3/8 1/4 3/8=40 \"allegro\"\r\n| ABC |\r\n"
remark = "r: this is a remark\r\n| ABC |\r\n"
rhythm = "R: Polska\r\n| ABC |\r\n"
source = "S: Christine Dyer\r\n| ABC |\r\n"
title = "T: Engelska efter Albert Augustsson\r\n| ABC |\r\n"
userDefined = "U: some comment\r\n| ABC |\r\n"
voice = "V: T1           clef=treble-8  name=\"Tenore I\"   snm=\"T.I\"\r\n| ABC |\r\n"
wordsAfter = "W: doh re mi fa \r\n| ABC |\r\n"
wordsAligned = "| ABC |\r\nw: doh re mi fa \r\n| ABC |\r\n"   -- only appears inline
reference = "X: 125\r\n| ABC |\r\n"
transcriber = "Z: John Watson\r\n| ABC |\r\n"
fieldContinuation = "R: Polska\r\n+: in triplet time\r\n| ABC |\r\n"
comment = "%%TBL:{\"version\":\"beta\",\"type\":\"tune\",\"id\":\"10294\"}\r\n| ABC |\r\n"
unsupportedHeader = "j: custom header\r\n| ABC |\r\n"
bracketInHeader = "r: this is a remark [part 1]\r\n| ABC |\r\n"


-- structure
inline = "| ABC z2 def z/ \r\nQ: 1/4=120\r\n| ABC z2 def z/ |\r\n"
inlineComment = "| ABC z2 def z/ \r\n%% this is a comment\r\n| ABC z2 def z/ |\r\n"

-- bad input
badChars1 = "| ABC z2 def z/ |\r\n| foo bar |\r\n"
badChars2 = "| foo bar |\r\n| ABC z2 def z/ |\r\n"
bracketInInlineHeader = "| ABC |\r\nr: this is a remark [part 1]\r\n"

-- canonical
canonicalised = "K: DMajor\r\n| ACF |\r\n"      -- C and F are implictly sharpened and should remain implicit in canonical
uncanonicalised = "K: DMajor\r\n| A^C^F |\r\n"  -- should be canonicalised to 'canonicalised'





