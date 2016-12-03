module Test.Octave exposing (tests)

import Test exposing (..)
import Expect exposing (..)
import Music.Octave exposing (..)
import Test.Utils exposing (assertMoveMatches)
import Debug exposing (..)


tests : Test
tests =
    describe "Octave Change"
        [ test "phrase 1 low to med" <|
            \() ->
                (assertMoveMatches
                    phrase1Low
                    up
                    phrase1Med
                )
        , test "phrase 1 med to high" <|
            \() ->
                (assertMoveMatches
                    phrase1Med
                    up
                    phrase1High
                )
        , test "phrase 1 med to low" <|
            \() ->
                (assertMoveMatches
                    phrase1Med
                    down
                    phrase1Low
                )
        , test "phrase 1 high to Med" <|
            \() ->
                (assertMoveMatches
                    phrase1High
                    down
                    phrase1Med
                )
        , test "phrase 2 low to med" <|
            \() ->
                (assertMoveMatches
                    phrase2Low
                    up
                    phrase2Med
                )
        , test "phrase 2 med to high" <|
            \() ->
                (assertMoveMatches
                    phrase2Med
                    up
                    phrase2High
                )
        , test "phrase 2 med to low" <|
            \() ->
                (assertMoveMatches
                    phrase2Med
                    down
                    phrase2Low
                )
        , test "phrase 2 high to Med" <|
            \() ->
                (assertMoveMatches
                    phrase2High
                    down
                    phrase2Med
                )
        ]


phrase1Low =
    "K: CMajor\x0D\n| A,B, (3CDE [FG] |\x0D\n"


phrase1Med =
    "K: CMajor\x0D\n| AB (3cde [fg] |\x0D\n"


phrase1High =
    "K: CMajor\x0D\n| ab (3c'd'e' [f'g'] |\x0D\n"


phrase2Low =
    "| A,,,B,,, C,D,E, [fg] |\x0D\n| a'>b' |\x0D\n"


phrase2Med =
    "| A,,B,, CDE [f'g'] |\x0D\n| a''>b'' |\x0D\n"


phrase2High =
    "| A,B, cde [f''g''] |\x0D\n| a'''>b''' |\x0D\n"
