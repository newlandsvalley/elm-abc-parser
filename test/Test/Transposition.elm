module Test.Transposition (tests) where

import ElmTest exposing (..)
import Music.Transposition exposing (..)
import Abc exposing (parse, parseError)
import Abc.ParseTree exposing (PitchClass(..), KeySignature, ModifiedKeySignature, Accidental(..), Mode(..), AbcNote)
import Abc.Canonical exposing (fromTune)
import Maybe exposing (Maybe)
import Result exposing (..)
import Ratio exposing (Rational, over, fromInt)

{- assert the transposed parsed input equals the target -}
assertTranspositionMatches : String -> ModifiedKeySignature -> String -> Assertion
assertTranspositionMatches s targetks target = 
  let 
    transposedResult = formatError (\_ -> "parse error") (parse s)
        `andThen` (\tune -> transposeTo targetks tune)
  in
    case transposedResult  of
      Ok res -> 
        assertEqual target (fromTune res)
      Err errs -> 
        assert False


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
        , test "FNat in GMaj to FMaj" (assertEqual
               (Ok eb)
               (transposeNote fMajor gMajor fnat)
               )
        , test "C# in AMaj to GMaj" (assertEqual
               (Ok b)
               (transposeNote gMajor aMajor cs)
               )       
        , test "C# in GMaj to AMaj" (assertEqual
               (Ok ds)
               (transposeNote aMajor gMajor cs)
               )
        , test "B in DMaj to CMaj" (assertEqual
               (Ok a)
               (transposeNote cMajor dMajor b)
               )
        ]
    phrases =
      suite "phrases"
        [ 
          test "C phrase to D phrase" (assertTranspositionMatches 
               cPhrase
               dMajor
               dPhrase
               ) 
        , test "D phrase to C phrase" (assertTranspositionMatches 
               dPhrase
               cMajor
               cPhrase
               )
        , test "C phrase to F phrase" (assertTranspositionMatches 
               cPhrase
               fMajor
               fPhrase
               ) 
        ]
{-
    single =
      suite "single test"
        [ 

         test "F phrase to C phrase" (assertTranspositionMatches 
               fPhrase
               cMajor
               cPhrase
               ) 
        ]
-}
    in
      suite "Music Transposition"
        [ 
        {- -}
          keys
        , notes
        , phrases
        {- -}
        -- single
        ]

-- note C Sharp and D Sharp are in octave 5 al the other notes are in octave 4 
cs : AbcNote
cs = { pitchClass = C,  accidental = Just Sharp, octave = 5, duration = fromInt 1, tied = False }

ds : AbcNote
ds = { pitchClass = D,  accidental = Just Sharp, octave = 5, duration = fromInt 1, tied = False }

eb : AbcNote
eb = { pitchClass = E,  accidental = Just Flat, octave = 4, duration = fromInt 1, tied = False }

b : AbcNote
b = { pitchClass = B,  accidental = Nothing, octave = 4, duration = fromInt 1, tied = False }

f : AbcNote
f = { pitchClass = F,  accidental = Nothing, octave = 4, duration = fromInt 1, tied = False }

fnat : AbcNote
fnat = { pitchClass = F,  accidental = Just Natural, octave = 4, duration = fromInt 1, tied = False }

g : AbcNote
g = { pitchClass = G,  accidental = Nothing, octave = 4, duration = fromInt 1, tied = False }

a : AbcNote
a = { pitchClass = A,  accidental = Nothing, octave = 4, duration = fromInt 1, tied = False }

fMajor : ModifiedKeySignature
fMajor = ({ pitchClass = F, accidental = Nothing, mode = Major }, [])

gMajor : ModifiedKeySignature
gMajor = ({ pitchClass = G, accidental = Nothing, mode = Major }, [])

aMajor : ModifiedKeySignature
aMajor = ({ pitchClass = A, accidental = Nothing, mode = Major }, [])

gSharpMajor : ModifiedKeySignature
gSharpMajor = ({ pitchClass = G, accidental = Just Sharp, mode = Major },[])

cMajor : ModifiedKeySignature
cMajor = ({ pitchClass = C, accidental = Nothing, mode = Major },[])

dMajor : ModifiedKeySignature
dMajor = ({ pitchClass = D, accidental = Nothing, mode = Major },[])

bFlatDorian : ModifiedKeySignature
bFlatDorian = ({ pitchClass = B, accidental = Just Flat, mode = Dorian },[])

bFlat : ModifiedKeySignature
bFlat = ({ pitchClass = B, accidental = Just Flat, mode = Major },[])


cPhrase = "K: CMajor\r\n| AB (3cde [fg] |\r\n"
dPhrase = "K: DMajor\r\n| Bc (3def [ga] |\r\n"
fPhrase = "K: FMajor\r\n| de (3fga [bc'] |\r\n"



