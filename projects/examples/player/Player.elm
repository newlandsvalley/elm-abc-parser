module Player exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, on, onCheck)
import Html.Attributes exposing (type_, checked)
import Http exposing (..)
import String exposing (..)
import Result exposing (Result, mapError)
import Melody exposing (..)
import AbcPerformance exposing (..)
import Repeats exposing (buildRepeatedMelody)
import RepeatTypes exposing (Repeats, Section)
import Abc.ParseTree exposing (..)
import Abc exposing (parse, parseError)


main =
    Html.program
        { init = ( init, Cmd.none ), update = update, view = view, subscriptions = \_ -> Sub.none }



-- MODEL


type alias Model =
    { performance : Result String ( MelodyLine, Repeats )
    , expandRepeats : Bool
    }


init : Model
init =
    { performance = Err "not started", expandRepeats = False }



-- UPDATE


type Msg
    = NoOp
    | ExpandRepeats Bool
    | Load String
    | RawAbc (Result Error String)
    | Abc (Result String ( MelodyLine, Repeats ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ExpandRepeats b ->
            ( { model | expandRepeats = b }, Cmd.none )

        RawAbc abc ->
            update (Abc (parseLoadedFile model.expandRepeats abc)) model

        Abc result ->
            ( { model | performance = result }, Cmd.none )

        Load url ->
            ( model, loadAbc url )



{- load an ABC file -}


loadAbc : String -> Cmd Msg
loadAbc url =
    Http.send RawAbc (getString url)


toPerformance : Bool -> Result String AbcTune -> Result String ( MelodyLine, Repeats )
toPerformance expandRepeats r =
    Result.map (melodyFromAbc expandRepeats) r


parseLoadedFile : Bool -> Result Error String -> Result String ( MelodyLine, Repeats )
parseLoadedFile expandRepeats s =
    case s of
        Ok text ->
            text
                |> parse
                |> mapError parseError
                |> toPerformance expandRepeats

        Err e ->
            Err (toString e)


viewPerformanceResult : Result String ( MelodyLine, Repeats ) -> String
viewPerformanceResult mr =
    case mr of
        Ok ( mel, rpts ) ->
            "OK: " ++ (toString mel) ++ "\nRepeats: " ++ (toString rpts)

        Err errs ->
            "Fail: " ++ (toString errs)


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick (Load "abc/lillasystern.abc") ] [ text "lillasystern" ]
        , button [ onClick (Load "abc/PolskaRattvik.abc") ] [ text "Rattvik polska" ]
        , button [ onClick (Load "abc/justnotes.abc") ] [ text "just notes" ]
        , button [ onClick (Load "abc/twobars.abc") ] [ text "two bars" ]
        , button [ onClick (Load "abc/baraccidental.abc") ] [ text "bar accidental" ]
        , button [ onClick (Load "abc/tie.abc") ] [ text "tie" ]
        , button [ onClick (Load "abc/chord.abc") ] [ text "chord" ]
        , button [ onClick (Load "abc/variantending.abc") ] [ text "variant ending" ]
        , button [ onClick (Load "abc/repeat1.abc") ] [ text "repeat 1" ]
        , button [ onClick (Load "abc/repeat2.abc") ] [ text "repeat 2" ]
        , button [ onClick (Load "abc/repeat3.abc") ] [ text "repeat 3" ]
        , button [ onClick (Load "abc/repeat4.abc") ] [ text "repeat 4" ]
        , button [ onClick (Load "abc/repeat5.abc") ] [ text "repeat 5" ]
        , button [ onClick (Load "abc/repeat6.abc") ] [ text "repeat 6" ]
        , button [ onClick (Load "abc/tripletaccidental.abc") ] [ text "triplet accidental" ]
        , label []
            [ br [] []
            , input [ type_ "checkbox", checked model.expandRepeats, onCheck ExpandRepeats ] []
            , text "expand repeats"
            ]
        , div [] [ text ("parse result: " ++ (viewPerformanceResult model.performance)) ]
        ]
