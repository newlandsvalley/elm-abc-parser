module Test.Abc (tests) where

import ElmTest exposing (..)
import Abc exposing (parse)
import Abc.ParseTree exposing (PitchClass(..), KeySignature, Accidental(..), Mode(..), AbcNote)
import Abc.Canonical exposing (fromTune)
import Maybe exposing (Maybe)


import String

roundTrip : String -> String
roundTrip s =
  let 
    parseResult = parse s
  in 
    case parseResult of
      Ok res -> 
        fromTune res
      Err errs -> 
        "Fail: " ++ errs

assertRoundTrip : String -> Assertion
assertRoundTrip s =
  assertEqual s (roundTrip s)

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
        , test "unit note length" (assertRoundTrip noteLength)
        , test "meter" (assertRoundTrip meter)
        , test "macro" (assertRoundTrip macro)
        , test "notes" (assertRoundTrip notes)
        , test "origin" (assertRoundTrip origin)
        , test "parts" (assertRoundTrip parts)
        , test "tempo" (assertRoundTrip tempo)
        , test "complex tempo" (assertRoundTrip tempoComplex)
        , test "rhythm" (assertRoundTrip rhythm)
        , test "source" (assertRoundTrip source)
        , test "title" (assertRoundTrip title)
        , test "user-defined" (assertRoundTrip userDefined)
        , test "voice" (assertRoundTrip voice)
        , test "words after" (assertRoundTrip wordsAfter)
        , test "words before" (assertRoundTrip wordsBefore)
        , test "reference number" (assertRoundTrip reference)
        , test "transcriber" (assertRoundTrip transcriber)
        , test "comment" (assertRoundTrip comment)
        ]
    tune =
      suite "tune"
        [ test "note" (assertRoundTrip note)
        , test "broken rhythm" (assertRoundTrip brokenRhythm)
        , test "accidentals" (assertRoundTrip accidentals)
        , test "octave" (assertRoundTrip octave)
        , test "tie" (assertRoundTrip tie)
        , test "repeat" (assertRoundTrip repeat)
        , test "triplet" (assertRoundTrip triplet)
        , test "tuplet" (assertRoundTrip triplet)
        , test "slur" (assertRoundTrip slur)
        , test "grace notes" (assertRoundTrip grace)
        , test "chord symbols" (assertRoundTrip chordSymbols)
        , test "chords" (assertRoundTrip chords)
        , test "articulation" (assertRoundTrip articulation)
        ]
  in
    suite "Music Notation"
      [  header
      ,  tune
      ]

-- these ABC samples must already be in canonical format for round-tripping to work
-- because of the exact string matching algorithm
note = "| ABC z2 def z/ |\r\n"
brokenRhythm = "| A>B C>>D a<b c<<d |\r\n"
accidentals = "| ^A_B c=d_e |\r\n"
octave = "| A,B,,C z2 d'e''f z/ |\r\n"
tie = "| A4~ A2 |\r\n"
repeat = "|: ABCD EFGa |1 D4 C4 :|2 c8 |\r\n"
triplet = "| (3de^f (3cda |\r\n"
tuplet = "| (3:2:4d2e2^fg |\r\n"
slur = "| (de^f) (cda) |\r\n"
grace = "| {d^f}GA |\r\n"
chordSymbols = "| \"Em\" EG \"Am\" AC |\r\n"
chords = "| [de^f]g [cda]b |\r\n"
articulation = "(vA2 | !fz!Ld2).d.f .e.d.c.B A2(A2 | d2).d.f .e.d.c.B A2A2 |\r\n"

area = "A: London\r\n| ABC |\r\n"
book = "B: Richie Robinson\r\n| ABC |\r\n"
composer = "C: Bys-Kalle\r\n| ABC |\r\n"
discography = "D: 2 Brudetstykke\r\n| ABC |\r\n"
fileurl = "F: http\\\\tradtunedb.org.uk\r\n| ABC |\r\n"
group = "G: Swåp\r\n| ABC |\r\n"
history = "H: Learned from AnnbjØrg Lien\r\n| ABC |\r\n"
instruction = "I: abc-charset UTF-8\r\n| ABC |\r\n"
noteLength = "L: 1/8\r\n| ABC |\r\n"
meter = "M: 3/4\r\n| ABC |\r\n"
macro = "m: ~g2 = {a}g{f}g\r\n| ABC |\r\n"
notes = "N: from recording made at Tideswell\r\n| ABC |\r\n"
origin = "O: Skåne\r\n| ABC |\r\n"
parts = "P: ((AB)3.(CD)3)2\r\n| ABC |\r\n"
tempo = "Q: 1/4=120\r\n| ABC |\r\n"
tempoComplex = "Q: 1/4 3/8 1/4 3/8=40 \"allegro\"\r\n| ABC |\r\n"
rhythm = "R: Polska\r\n| ABC |\r\n"
source = "S: Christine Dyer\r\n| ABC |\r\n"
title = "T: Engelska efter Albert Augustsson\r\n| ABC |\r\n"
userDefined = "U: some comment\r\n| ABC |\r\n"
voice = "V: T1           clef=treble-8  name=\"Tenore I\"   snm=\"T.I\"\r\n| ABC |\r\n"
wordsAfter = "W: doh re mi fa\r\n| ABC |\r\n"
wordsBefore = "w: doh re mi fa\r\n| ABC |\r\n"
reference = "X: 125\r\n| ABC |\r\n"
transcriber = "Z: John Watson\r\n| ABC |\r\n"
comment = "%%TBL:{\"version\":\"beta\",\"type\":\"tune\",\"id\":\"10294\"}\r\n| ABC |\r\n"






