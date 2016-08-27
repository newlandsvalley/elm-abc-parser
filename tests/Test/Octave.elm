module Test.Octave exposing 
  (tests)

import Test exposing (..)
import Expect exposing (..)
import Music.Octave exposing (..)
import Abc exposing (parse, parseError)
import Abc.ParseTree exposing (AbcTune)
import Abc.Canonical exposing (fromTune)
import Result exposing (..)

import Debug exposing (..)

{- assert the moved parsed input equals the target -}
assertMoveMatches : String -> (AbcTune -> AbcTune) -> String -> Expectation
assertMoveMatches s move target = 
  let 
    movedResult = formatError (\x -> "parse error: " ++ toString x) (parse s)
       |> Result.map move
  in
    case movedResult  of
      Ok res -> 
        Expect.equal target (fromTune res)
      Err errs -> 
        Expect.fail "unexpected error"

tests : Test
tests =
  describe "Octave Change"
    [ 
      test "phrase 1 low to med" <|
        \() -> (assertMoveMatches 
               phrase1Low
               up
               phrase1Med
               )          
    , test "phrase 1 med to high" <|
        \() -> (assertMoveMatches 
               phrase1Med
               up
               phrase1High
               )         
    , test "phrase 1 med to low" <|
        \() -> (assertMoveMatches 
               phrase1Med
               down
               phrase1Low
               )          
    , test "phrase 1 high to Med" <|
        \() -> (assertMoveMatches 
               phrase1High
               down
               phrase1Med
               )      
    , test "phrase 2 low to med" <|
        \() -> (assertMoveMatches 
               phrase2Low
               up
               phrase2Med
               )                  
    , test "phrase 2 med to high" <|
        \() -> (assertMoveMatches 
               phrase2Med
               up
               phrase2High
               )               
    , test "phrase 2 med to low" <|
        \() -> (assertMoveMatches 
               phrase2Med
               down
               phrase2Low
               )          
    , test "phrase 2 high to Med" <|
        \() -> (assertMoveMatches 
               phrase2High
               down
               phrase2Med
               )      
      ]

phrase1Low  = "K: CMajor\r\n| A,B, (3CDE [FG] |\r\n"
phrase1Med  = "K: CMajor\r\n| AB (3cde [fg] |\r\n"
phrase1High = "K: CMajor\r\n| ab (3c'd'e' [f'g'] |\r\n"

phrase2Low  = "| A,,,B,,, C,D,E, [fg] |\r\n| a'>b' |\r\n"
phrase2Med  = "| A,,B,, CDE [f'g'] |\r\n| a''>b'' |\r\n"
phrase2High = "| A,B, cde [f''g''] |\r\n| a'''>b''' |\r\n"






