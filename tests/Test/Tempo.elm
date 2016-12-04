module Test.Tempo exposing (tests)

import Test exposing (..)
import Expect exposing (..)
import Music.Tempo exposing (..)
import Test.Utils exposing (assertMoveMatches, assertIntFuncMatches)
import Result exposing (..)
import Debug exposing (..)


tests : Test
tests =
    describe "tempo change"
        [ test "get the tempo from header" <|
            \() ->
                (assertIntFuncMatches
                    fullHeaderHigh
                    getBpm
                    132
                )
        , test "get the default tempo when there's no header" <|
            \() ->
                (assertIntFuncMatches
                    noHeader
                    getBpm
                    120
                )
        , test "alter tempo of existing header" <|
            \() ->
                (assertMoveMatches
                    fullHeaderMed
                    (setBpm 132)
                    fullHeaderHigh
                )
        , test "new tempo from default of no headers" <|
            \() ->
                (assertMoveMatches
                    noHeader
                    (setBpm 144)
                    justTempoHeader
                )
        , test "new tempo from default of only Key header" <|
            \() ->
                (assertMoveMatches
                    onlyKeyHeader
                    (setBpm 84)
                    justTempoAndKeyHeader
                )
        ]


fullHeaderMed =
    "X: 1\x0D\nT: a title\x0D\nQ: 1/4=120\x0D\nM: 3/4\x0D\nK: CMajor\x0D\n| A,B, (3CDE [FG] |\x0D\n"


fullHeaderHigh =
    "X: 1\x0D\nT: a title\x0D\nM: 3/4\x0D\nQ: 1/4=132\x0D\nK: CMajor\x0D\n| A,B, (3CDE [FG] |\x0D\n"


noHeader =
    "| A,B, (3CDE [FG] |\x0D\n"


justTempoHeader =
    "Q: 1/4=144\x0D\n| A,B, (3CDE [FG] |\x0D\n"


onlyKeyHeader =
    "K: CMajor\x0D\n| A,B, (3CDE [FG] |\x0D\n"


justTempoAndKeyHeader =
    "Q: 1/4=84\x0D\nK: CMajor\x0D\n| A,B, (3CDE [FG] |\x0D\n"
