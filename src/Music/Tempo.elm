module Music.Tempo
    exposing
        ( defaultTempo
        , getBpm
        , setBpm
        )

{-| Get or set the tempo of a tune.

You can change the tempo of an ABC tune by altering its beats per minute (bpm).
For example, to set the tempo of a parsed tune to 80 bpm you can say:

* setBpm 80 tune



# Constants
@docs defaultTempo


# Functions
@docs getBpm
    , setBpm


-}

import Ratio exposing (Rational, over)
import Maybe exposing (withDefault)
import Dict exposing (Dict, fromList, get)
import Maybe.Extra exposing (isJust, or)
import Abc.ParseTree exposing (..)
import Music.Notation exposing (getHeaderMap)


-- Exposed API


{-| The default Tempo - 1/4=120.
-}
defaultTempo : TempoSignature
defaultTempo =
    { noteLengths = [ over 1 4 ]
    , bpm = 120
    , marking = Nothing
    }


{-| Get the tempo of the tune in beats per minute from the tunes header
    (if it exists) or the default of 120 if it does not.
-}
getBpm : AbcTune -> Int
getBpm tune =
    case (tempoHeader tune) of
        Tempo t ->
            t.bpm

        _ ->
            defaultTempo.bpm


{-| Change the tempo of the tune by altering the beats per minute (bpm)
   in the tune's tempo header (if it exists) or by altering a newly incorporated
   default tempo if not.
-}
setBpm : Int -> AbcTune -> AbcTune
setBpm bpm tune =
    let
        ( headers, body ) =
            tune

        newTempoHeader =
            -- case oldTempoHeader of
            case (tempoHeader tune) of
                Tempo t ->
                    Tempo { t | bpm = bpm }

                -- can't happen but type-checker can't know
                x ->
                    x

        newHeaders =
            replaceTempoHeader newTempoHeader headers
    in
        ( newHeaders, body )



-- implementation
{- get the tempo header -}


tempoHeader : AbcTune -> Header
tempoHeader tune =
    let
        ( headers, body ) =
            tune

        headerMap =
            getHeaderMap tune
    in
        Dict.get 'Q' headerMap
            |> withDefault (Tempo defaultTempo)



{- replace a tempo header (if it exists) -}


replaceTempoHeader : Header -> TuneHeaders -> TuneHeaders
replaceTempoHeader newTempoHeader hs =
    let
        f h =
            case h of
                Tempo _ ->
                    False

                _ ->
                    True

        newhs =
            List.filter f hs
    in
        -- newhs ++ [ Tempo newtempo ]
        placeHeaderPenultimately newTempoHeader newhs



{- the last ABC header should always be the key signature so we'll
   choose to set the (altered) tempo header as next-to-last.
-}


placeHeaderPenultimately : Header -> TuneHeaders -> TuneHeaders
placeHeaderPenultimately h hs =
    let
        reversedhs =
            List.reverse hs
    in
        case reversedhs of
            [] ->
                [ h ]

            x :: xs ->
                List.reverse (x :: h :: xs)
