module Main where

import Signal exposing (Signal)

import ElmTest exposing (..)
import Console exposing (IO, run)
import Task

import Test.Abc as Abc

all : Test
all =
    suite "ABC run one test"
    [ 
      Abc.singletest
    ]

console : IO ()
console = consoleRunner all

port runner : Signal (Task.Task x ())
port runner = run console
