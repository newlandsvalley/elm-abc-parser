module Test.Transposition (tests) where

import ElmTest exposing (..)
import Music.Transposition exposing (..)
import Abc.ParseTree exposing (PitchClass(..), KeySignature, Accidental(..), Mode(..), AbcNote)
import Maybe exposing (Maybe)
import Result exposing (..)
import Ratio exposing (Rational, over, fromInt)
import String


fNatural : AbcNote
fNatural = { pitchClass = F,  accidental = Nothing, octave = 4, duration = fromInt 1, tied = False }

gMajor : KeySignature
gMajor = { pitchClass = G, accidental = Nothing, mode = Major }

gSharpMajor : KeySignature
gSharpMajor = { pitchClass = G, accidental = Just Sharp, mode = Major }

cMajor : KeySignature
cMajor = { pitchClass = C, accidental = Nothing, mode = Major }

bFlatDorian : KeySignature
bFlatDorian = { pitchClass = B, accidental = Just Flat, mode = Dorian }

bFlat : KeySignature
bFlat = { pitchClass = B, accidental = Just Flat, mode = Major }

tests : Test
tests =
  let 
    keys =
      suite "keys"
        [ test "C to G#" (assertEqual
               (Ok 8)
               (keyDistance gSharpMajor cMajor)
               )
        , test "G# to Bb" (assertEqual
               (Ok 2)
               (keyDistance bFlat gSharpMajor)
               )
        , test "Bb to G#" (assertEqual
               (Ok -2)
               (keyDistance gSharpMajor bFlat)
               )
        , test "C to BbDor" (assertEqual
               (Err "incompatible modes")
               (keyDistance bFlatDorian cMajor)
               )
        ]
    in
      suite "Music Transposition"
        [ 
          keys
        ]


