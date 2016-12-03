module Tests exposing (..)

import Test exposing (..)
import Test.Abc exposing (tests)
import Test.Music exposing (tests)
import Test.Transposition exposing (tests)
import Test.Octave exposing (tests)
import Test.Tempo exposing (tests)


all : Test
all =
    concat
        [ Test.Abc.tests
        , Test.Music.tests
        , Test.Transposition.tests
        , Test.Octave.tests
        , Test.Tempo.tests
        ]
