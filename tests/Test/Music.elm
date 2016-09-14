module Test.Music exposing (tests)

import Test exposing (..)
import Expect exposing (..)
import Abc exposing (parse)
import Music.Notation exposing (..)
import Abc.ParseTree exposing (PitchClass(..), KeySignature, ModifiedKeySignature, Accidental(..), KeyAccidental, Mode(..), AbcNote, AbcTune)
import Ratio exposing (Rational, over, fromInt)
import String
import Dict exposing (keys)


expectEquivalentKeys : List KeyAccidental -> List KeyAccidental -> Expectation
expectEquivalentKeys actual expected =
    let
        f key acc =
            acc && (List.member key expected)
    in
        if (List.foldl f True actual) && (List.length actual == List.length expected) then
            Expect.pass
        else
            Expect.fail ("non-equivalent keys" ++ toString actual ++ toString expected)



{- assert there is no header in the tune as defined by the getf (get header) function -}


assertNoHeader : String -> (AbcTune -> Maybe h) -> Expectation
assertNoHeader source getf =
    let
        parseResult =
            parse source
    in
        case parseResult of
            Ok tune ->
                let
                    mtitle =
                        getf tune
                in
                    case mtitle of
                        Just title ->
                            Expect.fail "no title expected"

                        _ ->
                            Expect.pass

            _ ->
                Expect.fail "parse error"



{- assert there is a title in the tune and it equals the target -}


assertOkTitle : String -> String -> Expectation
assertOkTitle source target =
    let
        parseResult =
            parse source
    in
        case parseResult of
            Ok tune ->
                let
                    mtitle =
                        getTitle tune
                in
                    case mtitle of
                        Just title ->
                            Expect.equal target title

                        _ ->
                            Expect.fail "no title"

            _ ->
                Expect.fail "parse error"


assertHeaderCount : Int -> String -> Expectation
assertHeaderCount expectedCount source =
    let
        parseResult =
            parse source
    in
        case parseResult of
            Ok tune ->
                let
                    headerMap =
                        getHeaderMap tune

                    count =
                        headerMap
                            |> Dict.keys
                            |> List.length
                in
                    Expect.equal expectedCount count

            _ ->
                Expect.fail "parse error"



{- assert there is a key signature in the tune and it equals the target -}


assertOkKeySig : String -> ModifiedKeySignature -> Expectation
assertOkKeySig source target =
    let
        parseResult =
            parse source
    in
        case parseResult of
            Ok tune ->
                let
                    mkeySig =
                        getKeySig tune
                in
                    case mkeySig of
                        Just keySig ->
                            Expect.equal target keySig

                        _ ->
                            Expect.fail "no key signature"

            _ ->
                Expect.fail "parse error"


fNatural : AbcNote
fNatural =
    { pitchClass = F, accidental = Nothing, octave = 4, duration = fromInt 1, tied = False }


gMajor : KeySignature
gMajor =
    { pitchClass = G, accidental = Nothing, mode = Major }


gMinor : KeySignature
gMinor =
    { pitchClass = G, accidental = Nothing, mode = Minor }


cMajor : KeySignature
cMajor =
    { pitchClass = C, accidental = Nothing, mode = Major }


dMajor : KeySignature
dMajor =
    { pitchClass = D, accidental = Nothing, mode = Major }


fMajor : KeySignature
fMajor =
    { pitchClass = F, accidental = Nothing, mode = Major }


fMajorM : ModifiedKeySignature
fMajorM =
    ( fMajor, [] )


tests : Test
tests =
    let
        majorMode =
            describe "Major mode"
                [ test "G Major" <|
                    \() ->
                        (expectEquivalentKeys
                            (keySet { pitchClass = G, accidental = Nothing, mode = Major })
                            [ ( F, Sharp ) ]
                        )
                , test "Ab Major" <|
                    \() ->
                        (expectEquivalentKeys
                            (keySet { pitchClass = A, accidental = Just Flat, mode = Major })
                            [ ( B, Flat ), ( E, Flat ), ( A, Flat ), ( D, Flat ) ]
                        )
                , test "A Major" <|
                    \() ->
                        (expectEquivalentKeys
                            (keySet { pitchClass = A, accidental = Nothing, mode = Major })
                            [ ( C, Sharp ), ( F, Sharp ), ( G, Sharp ) ]
                        )
                , test "Bb Major" <|
                    \() ->
                        (expectEquivalentKeys
                            (keySet { pitchClass = B, accidental = Just Flat, mode = Major })
                            [ ( B, Flat ), ( E, Flat ) ]
                        )
                , test "B Major" <|
                    \() ->
                        (expectEquivalentKeys
                            (keySet { pitchClass = B, accidental = Nothing, mode = Major })
                            [ ( C, Sharp ), ( F, Sharp ), ( G, Sharp ), ( D, Sharp ), ( A, Sharp ) ]
                        )
                , test "C Major" <|
                    \() -> Expect.equal 0 (List.length (keySet { pitchClass = C, accidental = Nothing, mode = Major }))
                , test "Db Major" <|
                    \() ->
                        (expectEquivalentKeys
                            (keySet { pitchClass = D, accidental = Just Flat, mode = Major })
                            [ ( B, Flat ), ( E, Flat ), ( A, Flat ), ( D, Flat ), ( G, Flat ) ]
                        )
                , test "D Major" <|
                    \() ->
                        (expectEquivalentKeys
                            (keySet { pitchClass = D, accidental = Nothing, mode = Major })
                            [ ( F, Sharp ), ( C, Sharp ) ]
                        )
                , test "Eb Major" <|
                    \() ->
                        (expectEquivalentKeys
                            (keySet { pitchClass = E, accidental = Just Flat, mode = Major })
                            [ ( B, Flat ), ( E, Flat ), ( A, Flat ) ]
                        )
                , test "E Major" <|
                    \() ->
                        (expectEquivalentKeys
                            (keySet { pitchClass = E, accidental = Nothing, mode = Major })
                            [ ( C, Sharp ), ( F, Sharp ), ( G, Sharp ), ( D, Sharp ) ]
                        )
                , test "F Major" <|
                    \() ->
                        (expectEquivalentKeys
                            (keySet { pitchClass = F, accidental = Nothing, mode = Major })
                            [ ( B, Flat ) ]
                        )
                , test "F# Major" <|
                    \() ->
                        (expectEquivalentKeys
                            (keySet { pitchClass = F, accidental = Just Sharp, mode = Major })
                            [ ( C, Sharp ), ( F, Sharp ), ( G, Sharp ), ( D, Sharp ), ( A, Sharp ), ( E, Sharp ) ]
                        )
                , test "Gb Major" <|
                    \() ->
                        (expectEquivalentKeys
                            (keySet { pitchClass = G, accidental = Just Flat, mode = Major })
                            [ ( B, Flat ), ( E, Flat ), ( A, Flat ), ( D, Flat ), ( G, Flat ), ( C, Flat ) ]
                        )
                ]

        minorMode =
            describe "Minor mode"
                [ test "G Minor" <|
                    \() ->
                        (expectEquivalentKeys
                            (keySet { pitchClass = G, accidental = Nothing, mode = Minor })
                            [ ( B, Flat ), ( E, Flat ) ]
                        )
                , test "A Minor" <|
                    \() -> Expect.equal 0 (List.length (keySet { pitchClass = A, accidental = Nothing, mode = Minor }))
                ]

        klezmerMode =
            describe "Klezmer mode"
                [ test "D Phrygian with sharpened f" <|
                    \() ->
                        (expectEquivalentKeys
                            (modifiedKeySet ( { pitchClass = D, accidental = Nothing, mode = Phrygian }, [ ( F, Sharp ) ] ))
                            [ ( B, Flat ), ( E, Flat ), ( F, Sharp ) ]
                        )
                ]

        otherModes =
            describe "Other mode"
                [ test "C Doriam" <|
                    \() ->
                        (expectEquivalentKeys
                            (keySet { pitchClass = C, accidental = Nothing, mode = Dorian })
                            [ ( B, Flat ), ( E, Flat ) ]
                        )
                , test "D Dorian" <|
                    \() -> Expect.equal 0 (List.length (keySet { pitchClass = D, accidental = Nothing, mode = Dorian }))
                , test "C Phrygian" <|
                    \() ->
                        (expectEquivalentKeys
                            (keySet { pitchClass = C, accidental = Nothing, mode = Phrygian })
                            [ ( B, Flat ), ( E, Flat ), ( A, Flat ), ( D, Flat ) ]
                        )
                , test "E Phrygian" <|
                    \() -> Expect.equal 0 (List.length (keySet { pitchClass = E, accidental = Nothing, mode = Phrygian }))
                , test "C Lydian" <|
                    \() ->
                        (expectEquivalentKeys
                            (keySet { pitchClass = C, accidental = Nothing, mode = Lydian })
                            [ ( F, Sharp ) ]
                        )
                , test "F Lydian" <|
                    \() -> Expect.equal 0 (List.length (keySet { pitchClass = F, accidental = Nothing, mode = Lydian }))
                , test "C Mixolyydian" <|
                    \() ->
                        (expectEquivalentKeys
                            (keySet { pitchClass = C, accidental = Nothing, mode = Mixolydian })
                            [ ( B, Flat ) ]
                        )
                , test "G Lydian" <|
                    \() -> Expect.equal 0 (List.length (keySet { pitchClass = G, accidental = Nothing, mode = Mixolydian }))
                , test "C Aeolian" <|
                    \() ->
                        (expectEquivalentKeys
                            (keySet { pitchClass = C, accidental = Nothing, mode = Aeolian })
                            [ ( B, Flat ), ( E, Flat ), ( A, Flat ) ]
                        )
                , test "A Aeolian" <|
                    \() -> Expect.equal 0 (List.length (keySet { pitchClass = A, accidental = Nothing, mode = Aeolian }))
                , test "C Locrian" <|
                    \() ->
                        (expectEquivalentKeys
                            (keySet { pitchClass = C, accidental = Nothing, mode = Locrian })
                            [ ( B, Flat ), ( E, Flat ), ( A, Flat ), ( D, Flat ), ( G, Flat ) ]
                        )
                , test "B Locrian" <|
                    \() -> Expect.equal 0 (List.length (keySet { pitchClass = B, accidental = Nothing, mode = Locrian }))
                , test "G Ionian" <|
                    \() ->
                        (expectEquivalentKeys
                            (keySet { pitchClass = G, accidental = Nothing, mode = Ionian })
                            [ ( F, Sharp ) ]
                        )
                , test "C Ionian" <|
                    \() -> Expect.equal 0 (List.length (keySet { pitchClass = C, accidental = Nothing, mode = Major }))
                  -- need to replace these with fuzz tests
                  -- normalise currently fails with flat keys
                , test "normalise G Mix" <|
                    \() ->
                        let
                            sourceKey =
                                { pitchClass = G, accidental = Nothing, mode = Mixolydian }

                            targetKey =
                                normaliseModalKey sourceKey
                        in
                            expectEquivalentKeys (keySet sourceKey) (keySet targetKey)
                , test "normalise D Mix" <|
                    \() ->
                        let
                            sourceKey =
                                { pitchClass = D, accidental = Nothing, mode = Mixolydian }

                            targetKey =
                                normaliseModalKey sourceKey
                        in
                            expectEquivalentKeys (keySet sourceKey) (keySet targetKey)
                  -- FAILING TEST
                , test "normalise Bb Dor" <|
                    \() ->
                        let
                            sourceKey =
                                { pitchClass = B, accidental = Just Flat, mode = Dorian }

                            targetKey =
                                normaliseModalKey sourceKey
                        in
                            expectEquivalentKeys (keySet sourceKey) (keySet targetKey)
                ]

        lookups =
            describe "lookups"
                [ test "f in G Major" <|
                    \() ->
                        (Expect.equal
                            (Just Sharp)
                            (accidentalImplicitInKey F ( gMajor, [] ))
                        )
                , test "f in C Major" <|
                    \() ->
                        (Expect.equal
                            (Nothing)
                            (accidentalImplicitInKey F ( cMajor, [] ))
                        )
                ]

        keys =
            describe "keys"
                [ test "D is a sharp key" <|
                    \() ->
                        Expect.true
                            "D is sharp"
                            (isCOrSharpKey dMajor)
                , test "C is an (honourary) sharp key" <|
                    \() ->
                        Expect.true
                            "C is sharp (or C)"
                            (isCOrSharpKey cMajor)
                , test "F is not a sharp key" <|
                    \() ->
                        Expect.false
                            "F is sharp"
                            (isCOrSharpKey fMajor)
                , test "Gm is not a sharp key" <|
                    \() ->
                        Expect.false
                            "Gm is sharp"
                            (isCOrSharpKey gMinor)
                ]

        headers =
            describe "headers"
                [ -- tests for getTitle
                  test "OK Title header" <|
                    \() -> (assertOkTitle titledTune "Gamal Reinlender")
                , test "No Title header" <|
                    \() -> (assertNoHeader keyedTune getTitle)
                , test "First Title header" <|
                    \() -> (assertOkTitle doublyTitledTune "Nancy Dawson")
                  -- tests for getKeySig
                , test "OK key header" <|
                    \() -> (assertOkKeySig keyedTune fMajorM)
                , test "No Key header" <|
                    \() -> (assertNoHeader titledTune getKeySig)
                , test "multiple headers" <|
                    \() -> (assertHeaderCount 7 manyHeaders)
                ]
    in
        concat
            [ majorMode
            , minorMode
            , klezmerMode
            , otherModes
            , lookups
            , keys
            , headers
            ]



-- headers in sample ABC tunes


keyedTune =
    "K: FMajor\x0D\n| ABC |\x0D\n"


titledTune =
    "T: Gamal Reinlender\x0D\n| ABC |\x0D\n"


doublyTitledTune =
    "T: Nancy Dawson\x0D\nT: Piss Upon the Grass\x0D\n| ABC |\x0D\n"


manyHeaders =
    "X: 1\x0D\nT: Skänklåt efter Brittas Hans\x0D\nR: Skänklåt\x0D\nZ: Brian O'Connor, 11/7/2016\x0D\nO: Bjorsa\x0D\nM: 4/4\x0D\nK:Gmaj\x0D\n| ABC |\x0D\n"
