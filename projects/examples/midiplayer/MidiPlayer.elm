module MidiPlayer exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, on, onCheck)
import Html.Attributes exposing (type_, checked)
import Http exposing (..)
import Maybe exposing (withDefault)
import String exposing (..)
import Result exposing (Result, mapError)
import MidiMelody exposing (..)
import MidiTypes exposing (MidiRecording)
import MidiPerformance exposing (..)
import Repeats exposing (buildRepeatedMelody)
import RepeatTypes exposing (Repeats, Section)
import Abc.ParseTree exposing (..)
import Abc exposing (parse, parseError)


main =
    Html.program
        { init = ( init, Cmd.none ), update = update, view = view, subscriptions = \_ -> Sub.none }



-- MODEL


type alias Model =
    { abc : Result String AbcTune
    , url : Maybe String
    , expandRepeats : Bool
    , toMidiRecording : Bool
    }


init : Model
init =
    { abc = Err "not started", url = Nothing, expandRepeats = True, toMidiRecording = True }



-- UPDATE


type Msg
    = NoOp
    | ExpandRepeats Bool
    | MidiFormat Bool
    | Load String
    | RawAbc (Result Error String)
    | Abc (Result String AbcTune)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ExpandRepeats b ->
            let
                command =
                    case model.url of
                        Just url ->
                            loadAbc url

                        _ ->
                            Cmd.none
            in
                ( { model | expandRepeats = b }, command )

        MidiFormat b ->
            let
                command =
                    case model.url of
                        Just url ->
                            loadAbc url

                        _ ->
                            Cmd.none
            in
                ( { model | toMidiRecording = b }, command )

        RawAbc abc ->
            update (Abc (parseLoadedFile model.expandRepeats abc)) model

        Abc result ->
            ( { model | abc = result }, Cmd.none )

        Load url ->
            ( { model | url = Just url }, loadAbc url )



{- load an ABC file -}


loadAbc : String -> Cmd Msg
loadAbc url =
    Http.send RawAbc (getString url)


toPerformance : Bool -> Result String AbcTune -> Result String ( MidiMelody, Repeats )
toPerformance expandRepeats r =
    Result.map (melodyFromAbc expandRepeats) r


toMidiRecording : Bool -> Result String AbcTune -> Result String MidiRecording
toMidiRecording expandRepeats r =
    Result.map (midiRecordingFromAbc expandRepeats) r


parseLoadedFile : Bool -> Result Error String -> Result String AbcTune
parseLoadedFile expandRepeats s =
    case s of
        Ok text ->
            text
                |> parse
                |> mapError parseError

        Err e ->
            Err (toString e)


viewPerformanceResult : Result String ( MidiMelody, Repeats ) -> String
viewPerformanceResult mr =
    case mr of
        Ok ( mel, rpts ) ->
            "OK: " ++ (toString mel) ++ "\nRepeats: " ++ (toString rpts)

        Err errs ->
            "Fail: " ++ (toString errs)


viewMidiRecordingResult : Result String MidiRecording -> String
viewMidiRecordingResult mr =
    case mr of
        Ok recording ->
            let
                ( header, tracks ) =
                    recording

                track0 =
                    List.head tracks
                        |> withDefault []
            in
                "OK: " ++ "header: " ++ (toString header) ++ " track0: " ++ (toString track0)

        Err errs ->
            "Fail: " ++ (toString errs)


result : Model -> String
result model =
    if model.toMidiRecording then
        viewMidiRecordingResult (toMidiRecording model.expandRepeats model.abc)
    else
        viewPerformanceResult (toPerformance model.expandRepeats model.abc)


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
        , label []
            [ br [] []
            , input [ type_ "checkbox", checked model.expandRepeats, onCheck ExpandRepeats ] []
            , text "expand repeats"
            ]
        , label []
            [ br [] []
            , input [ type_ "checkbox", checked model.toMidiRecording, onCheck MidiFormat ] []
            , text "to midi recording"
            ]
        , div [] [ text ("parse result: " ++ (result model)) ]
        ]
