module Test.Transposition (tests) where

import ElmTest exposing (..)
import Music.Transposition exposing (..)
import Abc.ParseTree exposing (PitchClass(..), KeySignature, Accidental(..), Mode(..), AbcNote)
import Maybe exposing (Maybe)
import Result exposing (..)
import Ratio exposing (Rational, over, fromInt)
import String

-- note c sharp is in octave 5 al the other notes are in octave 4 
cs : AbcNote
cs = { pitchClass = C,  accidental = Just Sharp, octave = 5, duration = fromInt 1, tied = False }

eb : AbcNote
eb = { pitchClass = E,  accidental = Just Flat, octave = 4, duration = fromInt 1, tied = False }

b : AbcNote
b = { pitchClass = B,  accidental = Nothing, octave = 4, duration = fromInt 1, tied = False }

f : AbcNote
f = { pitchClass = F,  accidental = Nothing, octave = 4, duration = fromInt 1, tied = False }

g : AbcNote
g = { pitchClass = G,  accidental = Nothing, octave = 4, duration = fromInt 1, tied = False }

fMajor : KeySignature
fMajor = { pitchClass = F, accidental = Nothing, mode = Major }

gMajor : KeySignature
gMajor = { pitchClass = G, accidental = Nothing, mode = Major }

aMajor : KeySignature
aMajor = { pitchClass = A, accidental = Nothing, mode = Major }

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
    notes =
      suite "notes"
        [ test "F in FMaj to GMaj" (assertEqual
               (Ok g)
               (transposeNote gMajor fMajor f)
               )
        , test "F in GMaj to FMaj" (assertEqual
               (Ok eb)
               (transposeNote fMajor gMajor f)
               )
        , test "C# in AMaj to GMaj" (assertEqual
               (Ok b)
               (transposeNote gMajor aMajor cs)
               )
        ]
    in
      suite "Music Transposition"
        [ 
          keys
        , notes
        ]


