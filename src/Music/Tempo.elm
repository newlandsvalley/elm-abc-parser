module Music.Tempo
    exposing
        ( defaultTempo
        , changeBpm
        )

{-| Tune tempo changes

You can change the tempo of an ABC tune by altering its beats per minute (bpm).
For example, to set the tempo of a parsed tune to 80 bpm you can say:

* changeBpm 80 tune



# Constants
@docs defaultTempo


# Functions
@docs changeBpm


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


{-| Change the tempo of the tune by altering the beats per minute (bpm)
   in the tune's tempo header (if it exists) or by altering a newly incorporated
   default tempo if not.
-}
changeBpm : Int -> AbcTune -> AbcTune
changeBpm bpm tune =
    let
        ( headers, body ) =
            tune

        headerMap =
            getHeaderMap tune

        -- Tempo is the 'Q' header
        oldTempoHeader =
            Dict.get 'Q' headerMap
                |> withDefault (Tempo defaultTempo)

        newTempoHeader =
            case oldTempoHeader of
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
