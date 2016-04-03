module Test.Transposition (tests) where

import ElmTest exposing (..)
import Music.Transposition exposing (..)
import Abc exposing (parse, parseError)
import Abc.ParseTree exposing (PitchClass(..), KeySignature, ModifiedKeySignature, Accidental(..), Mode(..), AbcNote)
import Abc.Canonical exposing (fromTune)
import Maybe exposing (Maybe)
import Result exposing (..)
import Ratio exposing (Rational, over, fromInt)

import Debug exposing (..)

{- assert the transposed parsed input equals the target -}
assertTranspositionMatches : String -> ModifiedKeySignature -> String -> Assertion
assertTranspositionMatches s targetks target = 
  let 
    transposedResult = formatError (\x -> "parse error: " ++ toString x) (parse s)
        `andThen` (\tune -> transposeTo targetks tune)
  in
    case transposedResult  of
      Ok res -> 
        assertEqual target (fromTune res)
      Err errs -> 
        let 
           _ = log "unexpected error" errs
        in
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
        , test "Gm phrase to Dm phrase" (assertTranspositionMatches 
               gmPhrase
               dMinor
               dmPhrase
               ) 
        , test "Gm phrase with in-bar accidental" (assertTranspositionMatches 
               gmPhraseLocal
               dMinor
               dmPhrase
               ) 
        , test "Dm phrase to Gm phrase" (assertTranspositionMatches 
               dmPhrase
               gMinor
               gmPhrase
               )  
        ]
    keyChanges =  
      suite "inline key changes"
        [
         test "key change Bm to Am" (assertTranspositionMatches 
               keyChangeBm 
               aMinor
               keyChangeAm
               ) 
        ]
{-
    single =
      suite "single test"
        [ 

          test "Gm phrase with in-bar accidental" (assertTranspositionMatches 
               gmPhraseLocal
               dMinor
               dmPhrase
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
        , keyChanges
        {- -}
        -- single
        ]

-- note C Sharp and D Sharp are in octave 5 all the other notes are in octave 4 
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

gMinor : ModifiedKeySignature
gMinor = ({ pitchClass = G, accidental = Nothing, mode = Minor }, [])

aMinor : ModifiedKeySignature
aMinor = ({ pitchClass = A, accidental = Nothing, mode = Minor }, [])

aMajor : ModifiedKeySignature
aMajor = ({ pitchClass = A, accidental = Nothing, mode = Major }, [])

gSharpMajor : ModifiedKeySignature
gSharpMajor = ({ pitchClass = G, accidental = Just Sharp, mode = Major },[])

cMajor : ModifiedKeySignature
cMajor = ({ pitchClass = C, accidental = Nothing, mode = Major },[])

dMajor : ModifiedKeySignature
dMajor = ({ pitchClass = D, accidental = Nothing, mode = Major },[])

dMinor : ModifiedKeySignature
dMinor = ({ pitchClass = D, accidental = Nothing, mode = Minor },[])

bFlatDorian : ModifiedKeySignature
bFlatDorian = ({ pitchClass = B, accidental = Just Flat, mode = Dorian },[])

bFlat : ModifiedKeySignature
bFlat = ({ pitchClass = B, accidental = Just Flat, mode = Major },[])


cPhrase = "K: CMajor\r\n| AB (3cde [fg] |\r\n"
dPhrase = "K: DMajor\r\n| Bc (3def [ga] |\r\n"
fPhrase = "K: FMajor\r\n| de (3fga [bc'] |\r\n"
gmPhrase = "K: GMinor\r\n| G3A B6 Ac |\r\n B2AG ^FGA^F D4\r\n"
gmPhraseLocal = "K: GMinor\r\n| G3A B6 Ac |\r\n B2AG ^FGAF D4\r\n"  -- second F implicitly sharpened
dmPhrase = "K: DMinor\r\n| D3E F6 EG |\r\n F2ED ^CDE^C A,4\r\n"

keyChangeBm = "K: BMinor\r\n| B4 A4 B4 | d2f2 e2dc c2d2 |\r\nK: F#Minor\r\n| f4 e4 f4 | g2a2 b2ag g2a2 |\r\n"
keyChangeAm = "K: AMinor\r\n| A4 G4 A4 | c2e2 d2cB B2c2 |\r\nK: EMinor\r\n| e4 d4 e4 | f2g2 a2gf f2g2 |\r\n"



