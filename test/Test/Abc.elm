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
    parserFeature =
      suite "parser"
        [ test "notes" (assertRoundTrip notes)
        , test "broken rhythm" (assertRoundTrip brokenRhythm)
        , test "accidentals" (assertRoundTrip accidentals)
        , test "octave" (assertRoundTrip octave)
        ]
  in
    suite "Music Notation"
      [ parserFeature
      ]

-- these ABC samples must already be in canonical format for round-tripping to work
-- because of the exact string matching algorithm
notes = "| ABC z2 def z/ |\r\n"
brokenRhythm = "| A>B C>>D a<b c<<d |\r\n"
accidentals = "| ^A_B c=d_e |\r\n"
octave = "| A,B,,C z2 d'e''f z/ |\r\n"
