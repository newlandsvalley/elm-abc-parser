module Main where

import Signal exposing (Signal)

import ElmTest exposing (..)
import Console exposing (IO, run)
import Task

import Test.Music as Music

all : Test
all =
    suite "ABC parser tests"
    [ Music.tests
    ]

console : IO ()
console = consoleRunner all

port runner : Signal (Task.Task x ())
port runner = run console
