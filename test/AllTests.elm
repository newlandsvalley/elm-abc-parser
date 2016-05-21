module AllTests exposing (..)

import ElmTest exposing (..)

import Test.Music as Music
import Test.Transposition as Transposition
import Test.Octave as Octave
import Test.Abc as Abc

all : Test
all =
    suite "ABC parser tests"
    [ 
      Abc.tests
    , Music.tests
    , Transposition.tests
    , Octave.tests
    ]

main : Program Never
main =
  runSuite all
