module Test.Utils exposing (..)

import Test exposing (..)
import Expect exposing (..)
import Abc exposing (parse, parseError)
import Abc.ParseTree exposing (AbcTune)
import Abc.Canonical exposing (fromTune)
import Result exposing (..)


{- assert the moved parsed input equals the target -}


assertMoveMatches : String -> (AbcTune -> AbcTune) -> String -> Expectation
assertMoveMatches s move target =
    let
        movedResult =
            mapError (\x -> "parse error: " ++ toString x) (parse s)
                |> Result.map move
    in
        case movedResult of
            Ok res ->
                Expect.equal target (fromTune res)

            Err errs ->
                Expect.fail "unexpected error"
