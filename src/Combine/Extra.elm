module Combine.Extra
    exposing
        ( manyTill1
        , leftBiasedOr
        )

{-| Extension functions to elm-combine to allow custom error-reporting

# Definition

# Functions
@docs manyTill1
    , leftBiasedOr

-}

import Combine exposing (..)
import Combine.Char exposing (..)


-- import Combine.Infix exposing (..)
{- Provide a version of manyTill that preserves the error position of the 'many' rather than of the 'end'
   Many thanks to Bogdan Papa for helping me with this

   manyTill' Parser res -> Parser end -> Parser (List res)
-}


manyTill1 : Parser s a -> Parser s x -> Parser s (List a)
manyTill1 p end =
    let
        accumulate acc state stream =
            case app end state stream of
                ( rstate, rstream, Ok _ ) ->
                    ( rstate, rstream, Ok (List.reverse acc) )

                _ ->
                    case app p state stream of
                        ( rstate, rstream, Ok res ) ->
                            accumulate (res :: acc) rstate rstream

                        ( estate, estream, Err ms ) ->
                            ( estate, estream, Err ms )
    in
        primitive <| accumulate []



{-
   manyTill1 p end =
       let
           accumulate acc cx =
               case app end cx of
                   -- We've reached the end so we return the accumulated results
                   -- and the context _after_ running the `end` parser.
                   ( Ok _, rcx ) ->
                       ( Ok (List.reverse acc), rcx )

                   ( Err _, origcx ) ->
                       case app p cx of
                           -- Our parser succeeded so we loop
                           ( Ok res, rcx ) ->
                               accumulate (res :: acc) rcx

                           -- Our parser failed so we fail the whole thing. This is
                           -- where this implementation differs from the standard
                           -- `manyTill`, we return `p`'s error rather than `end`'s.
                           ( Err ms, ecx ) ->
                               ( Err ms, ecx )


       in
           primitive <| accumulate []
-}
{- Provide a version of or that preserves the error position of the left hand branch rather than
   the start of the 'orred' construction.  Used to ensure manyTill' error positions filter up through 'or'

   leftBiasedOr : Parser res -> Parser res -> Parser res
-}


leftBiasedOr : Parser s a -> Parser s a -> Parser s a
leftBiasedOr lp rp =
    --  or lp rp
    let
        f state cx =
            let
                res =
                    app lp state cx
            in
                case res of
                    ( lstate, lstream, Ok _ ) ->
                        res

                    ( estate, lcx, Err lm ) ->
                        let
                            res1 =
                                app rp state cx
                        in
                            case res1 of
                                ( rstate, rstream, Ok _ ) ->
                                    res1

                                ( estate, rcx, Err rm ) ->
                                    ( estate, lcx, Err (lm ++ rm) )
    in
        primitive <| f
