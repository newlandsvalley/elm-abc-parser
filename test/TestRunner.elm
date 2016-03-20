module Main where

import Signal exposing (Signal)

import ElmTest exposing (..)
import Console exposing (IO, run)
import Task

import Test.Music as Music
import Test.Transposition as Transposition
import Test.Abc as Abc

all : Test
all =
    suite "ABC parser tests"
    [ 
      Abc.tests
    , Music.tests
    , Transposition.tests
    ]

console : IO ()
console = consoleRunner all

port runner : Signal (Task.Task x ())
port runner = run console
