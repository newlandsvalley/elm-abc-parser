module Test.Transposition exposing (tests, singletest)

import Test exposing (..)
import Expect exposing (..)
import Music.Transposition exposing (..)
import Abc exposing (parse, parseError)
import Abc.ParseTree exposing (PitchClass(..), KeySignature, ModifiedKeySignature, Accidental(..), Mode(..), AbcNote)
import Abc.Canonical exposing (fromTune)
import Result exposing (..)
import Ratio exposing (Rational, over, fromInt)
import Debug exposing (..)


{- assert the transposed parsed input equals the target -}


assertTranspositionMatches : String -> ModifiedKeySignature -> String -> Expectation
assertTranspositionMatches s targetks target =
    let
        transposedResult =
            formatError (\x -> "parse error: " ++ toString x) (parse s)
                `andThen` (\tune -> transposeTo targetks tune)
    in
        case transposedResult of
            Ok res ->
                Expect.equal target (fromTune res)

            Err errs ->
                let
                    _ =
                        log "unexpected error" errs
                in
                    Expect.fail "unexpected transposition error"


tests : Test
tests =
    let
        keys =
            describe "keys"
                [ test "C to G#" <|
                    \() ->
                        Expect.equal
                            (Ok 8)
                            (keyDistance gSharpMajor cMajor)
                , test "G# to Bb" <|
                    \() ->
                        Expect.equal
                            (Ok 2)
                            (keyDistance bFlat gSharpMajor)
                , test "Bb to G#" <|
                    \() ->
                        Expect.equal
                            (Ok -2)
                            (keyDistance gSharpMajor bFlat)
                , test "C to BbDor" <|
                    \() ->
                        Expect.equal
                            (Err "incompatible modes")
                            (keyDistance bFlatDorian cMajor)
                ]

        notes =
            describe "notes"
                [ test "F in FMaj to GMaj" <|
                    \() ->
                        Expect.equal
                            (Ok g)
                            (transposeNote gMajor fMajor f)
                , test "FNat in GMaj to FMaj" <|
                    \() ->
                        Expect.equal
                            (Ok eb)
                            (transposeNote fMajor gMajor fnat)
                , test "C# in AMaj to GMaj" <|
                    \() ->
                        Expect.equal
                            (Ok b)
                            (transposeNote gMajor aMajor cs)
                , test "C# in GMaj to AMaj" <|
                    \() ->
                        Expect.equal
                            (Ok ds)
                            (transposeNote aMajor gMajor cs)
                  -- should produce an explicit e natural because there is no e natural in the diatonic scale of FMin
                , test "G# in Amin to FMin" <|
                    \() ->
                        Expect.equal
                            (Ok enat)
                            (transposeNote fMinor aMinor gs)
                , test "B in DMaj to CMaj" <|
                    \() ->
                        Expect.equal
                            (Ok a)
                            (transposeNote cMajor dMajor b)
                ]

        phrases =
            describe "phrases"
                [ test "C phrase to D phrase" <|
                    \() ->
                        assertTranspositionMatches
                            cPhrase
                            dMajor
                            dPhrase
                , test "D phrase to C phrase" <|
                    \() ->
                        assertTranspositionMatches
                            dPhrase
                            cMajor
                            cPhrase
                , test "C phrase to F phrase" <|
                    \() ->
                        assertTranspositionMatches
                            cPhrase
                            fMajor
                            fPhrase
                , test "Gm phrase to Dm phrase" <|
                    \() ->
                        assertTranspositionMatches
                            gmPhrase
                            dMinor
                            dmPhrase
                , test "Gm phrase with in-bar accidental" <|
                    \() ->
                        assertTranspositionMatches
                            gmPhraseLocal
                            dMinor
                            dmPhrase
                , test "Dm phrase to Gm phrase" <|
                    \() ->
                        assertTranspositionMatches
                            dmPhrase
                            gMinor
                            gmPhraseLocal
                , test "Bm phrase to Em phrase" <|
                    \() ->
                        assertTranspositionMatches
                            bmPhrase
                            eMinor
                            emPhrase
                , test "Am phrase to Fm phrase" <|
                    \() ->
                        assertTranspositionMatches
                            amPhrase
                            fMinor
                            fmPhrase
                , test "Am phrase to F#m phrase" <|
                    \() ->
                        assertTranspositionMatches
                            amPhrase0
                            fSharpMinor
                            fsharpmPhrase0
                , test "identity transposition" <|
                    \() ->
                        assertTranspositionMatches
                            dmPhrase
                            dMinor
                            dmPhrase
                , test "Cm phrase to Am phrase" <|
                    \() ->
                        assertTranspositionMatches
                            cmPhrase1
                            aMinor
                            amPhrase1High
                ]

        keyChanges =
            describe "inline key changes"
                [ test "key change Bm to Am" <|
                    \() ->
                        assertTranspositionMatches
                            keyChangeBm
                            aMinor
                            keyChangeAm
                , test "key change Am to Bm" <|
                    \() ->
                        assertTranspositionMatches
                            keyChangeAm
                            bMinor
                            keyChangeBm
                , test "key change Bm to Em" <|
                    \() ->
                        assertTranspositionMatches
                            keyChangeBm
                            eMinor
                            keyChangeEmHigh
                , test "key change Em to Bm" <|
                    \() ->
                        assertTranspositionMatches
                            keyChangeEm
                            bMinor
                            keyChangeBm
                , test "key change Bm to C#m" <|
                    \() ->
                        assertTranspositionMatches
                            keyChangeBm
                            cSharpMinor
                            keyChangeCSharpmHigh
                , test "key change C#m to Bm" <|
                    \() ->
                        assertTranspositionMatches
                            keyChangeCSharpm
                            bMinor
                            keyChangeBm
                , test "key change Bm to Am inline" <|
                    \() ->
                        assertTranspositionMatches
                            keyChangeBmInline
                            aMinor
                            keyChangeAmInline
                ]
    in
        concat
            [ keys
            , notes
            , phrases
            , keyChanges
            ]



-- a simple wrapper for running a single test


singletest : Test
singletest =
    describe "single test"
        [ test "fm phrase to Am phrase" <|
            \() ->
                assertTranspositionMatches
                    fmPhrase1
                    aMinor
                    amPhrase1
        ]



-- note C Sharp and D Sharp are in octave 5 all the other notes are in octave 4


cs : AbcNote
cs =
    { pitchClass = C, accidental = Just Sharp, octave = 5, duration = fromInt 1, tied = False }


ds : AbcNote
ds =
    { pitchClass = D, accidental = Just Sharp, octave = 5, duration = fromInt 1, tied = False }


eb : AbcNote
eb =
    { pitchClass = E, accidental = Just Flat, octave = 4, duration = fromInt 1, tied = False }


enat : AbcNote
enat =
    { pitchClass = E, accidental = Just Natural, octave = 4, duration = fromInt 1, tied = False }


b : AbcNote
b =
    { pitchClass = B, accidental = Nothing, octave = 4, duration = fromInt 1, tied = False }


bnat : AbcNote
bnat =
    { pitchClass = B, accidental = Just Natural, octave = 4, duration = fromInt 1, tied = False }


f : AbcNote
f =
    { pitchClass = F, accidental = Nothing, octave = 4, duration = fromInt 1, tied = False }


fnat : AbcNote
fnat =
    { pitchClass = F, accidental = Just Natural, octave = 4, duration = fromInt 1, tied = False }


g : AbcNote
g =
    { pitchClass = G, accidental = Nothing, octave = 4, duration = fromInt 1, tied = False }


gs : AbcNote
gs =
    { pitchClass = G, accidental = Just Sharp, octave = 4, duration = fromInt 1, tied = False }


a : AbcNote
a =
    { pitchClass = A, accidental = Nothing, octave = 4, duration = fromInt 1, tied = False }


fMajor : ModifiedKeySignature
fMajor =
    ( { pitchClass = F, accidental = Nothing, mode = Major }, [] )


fMinor : ModifiedKeySignature
fMinor =
    ( { pitchClass = F, accidental = Nothing, mode = Minor }, [] )


fSharpMinor : ModifiedKeySignature
fSharpMinor =
    ( { pitchClass = F, accidental = Just Sharp, mode = Minor }, [] )


gMajor : ModifiedKeySignature
gMajor =
    ( { pitchClass = G, accidental = Nothing, mode = Major }, [] )


gMinor : ModifiedKeySignature
gMinor =
    ( { pitchClass = G, accidental = Nothing, mode = Minor }, [] )


aMajor : ModifiedKeySignature
aMajor =
    ( { pitchClass = A, accidental = Nothing, mode = Major }, [] )


aMinor : ModifiedKeySignature
aMinor =
    ( { pitchClass = A, accidental = Nothing, mode = Minor }, [] )


bMinor : ModifiedKeySignature
bMinor =
    ( { pitchClass = B, accidental = Nothing, mode = Minor }, [] )


gSharpMajor : ModifiedKeySignature
gSharpMajor =
    ( { pitchClass = G, accidental = Just Sharp, mode = Major }, [] )


cMajor : ModifiedKeySignature
cMajor =
    ( { pitchClass = C, accidental = Nothing, mode = Major }, [] )


cSharpMinor : ModifiedKeySignature
cSharpMinor =
    ( { pitchClass = C, accidental = Just Sharp, mode = Minor }, [] )


dMajor : ModifiedKeySignature
dMajor =
    ( { pitchClass = D, accidental = Nothing, mode = Major }, [] )


dMinor : ModifiedKeySignature
dMinor =
    ( { pitchClass = D, accidental = Nothing, mode = Minor }, [] )


eMinor : ModifiedKeySignature
eMinor =
    ( { pitchClass = E, accidental = Nothing, mode = Minor }, [] )


bFlatDorian : ModifiedKeySignature
bFlatDorian =
    ( { pitchClass = B, accidental = Just Flat, mode = Dorian }, [] )


bFlat : ModifiedKeySignature
bFlat =
    ( { pitchClass = B, accidental = Just Flat, mode = Major }, [] )


cPhrase =
    "K: CMajor\x0D\n| AB (3cde [fg] |\x0D\n"


dPhrase =
    "K: DMajor\x0D\n| Bc (3def [ga] |\x0D\n"


fPhrase =
    "K: FMajor\x0D\n| de (3fga [bc'] |\x0D\n"


gmPhrase =
    "K: GMinor\x0D\n| G3A B6 Ac |\x0D\n B2AG ^FGA^F D4\x0D\n"


gmPhraseLocal =
    "K: GMinor\x0D\n| G3A B6 Ac |\x0D\n B2AG ^FGAF D4\x0D\n"



-- second F implicitly sharpened


dmPhrase =
    "K: DMinor\x0D\n| D3E F6 EG |\x0D\n F2ED ^CDEC A,4\x0D\n"


bmPhrase =
    "K: BMinor\x0D\n| B4 A4 B4 | c2d2 e2dc c2d2 |\x0D\n"


emPhrase =
    "K: EMinor\x0D\n| e4 d4 e4 | f2g2 a2gf f2g2 |\x0D\n"


amPhrase0 =
    "K: AMinor\x0D\n| edcB A2E2 C2E2 | A^GAB cBcd e4 |\x0D\n"


fsharpmPhrase0 =
    "K: F#Minor\x0D\n| cBAG F2C2 A,2C2 | F=F^FG AGAB c4 |\x0D\n"


amPhrase1High =
    "K: AMinor\x0D\n| c'2ba ^gabg e4 |\x0D\n"


amPhrase1 =
    "K: AMinor\x0D\n| c2BA ^GABG E4 |\x0D\n"


cmPhrase1 =
    "K: CMinor\x0D\n| e2dc =BcdB G4 |\x0D\n"


fmPhrase1 =
    "K: FMinor\x0D\n| A2GF =EFGE C4 |\x0D\n"


amPhrase =
    "K: AMinor\x0D\n| e2ef g2gf e2ed | c2ce d2dB c4 |\x0D\n"


fmPhrase =
    "K: FMinor\x0D\n| c2c^c e2ec =c2cB | A2Ac B2BG A4 |\x0D\n"


keyChangeBm =
    "K: BMinor\x0D\n| B4 A4 B4 | d2f2 e2dc c2d2 |\x0D\nK: F#Minor\x0D\n| f4 e4 f4 | g2a2 b2ag g2a2 |\x0D\n"


keyChangeAm =
    "K: AMinor\x0D\n| A4 G4 A4 | c2e2 d2cB B2c2 |\x0D\nK: EMinor\x0D\n| e4 d4 e4 | f2g2 a2gf f2g2 |\x0D\n"


keyChangeEm =
    "K: EMinor\x0D\n| E4 D4 E4 | G2B2 A2GF F2G2 |\x0D\nK: BMinor\x0D\n| B4 A4 B4 | c2d2 e2dc c2d2 |\x0D\n"


keyChangeEmHigh =
    "K: EMinor\x0D\n| e4 d4 e4 | g2b2 a2gf f2g2 |\x0D\nK: BMinor\x0D\n| b4 a4 b4 | c'2d'2 e'2d'c' c'2d'2 |\x0D\n"


keyChangeCSharpm =
    "K: C#Minor\x0D\n| C4 B,4 C4 | E2G2 F2ED D2E2 |\x0D\nK: G#Minor\x0D\n| G4 F4 G4 | A2B2 c2BA A2B2 |\x0D\n"


keyChangeCSharpmHigh =
    "K: C#Minor\x0D\n| c4 B4 c4 | e2g2 f2ed d2e2 |\x0D\nK: G#Minor\x0D\n| g4 f4 g4 | a2b2 c'2ba a2b2 |\x0D\n"


keyChangeBmInline =
    "K: BMinor\x0D\n| B4 A4 B4 | d2f2 e2dc c2d2 | [K: F#Minor] f4 e4 f4 | g2a2 b2ag g2a2 |\x0D\n"


keyChangeAmInline =
    "K: AMinor\x0D\n| A4 G4 A4 | c2e2 d2cB B2c2 | [K: EMinor] e4 d4 e4 | f2g2 a2gf f2g2 |\x0D\n"
