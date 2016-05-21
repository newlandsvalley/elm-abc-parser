module Player exposing (..)


import Html exposing (..)
import Html.Events exposing (onClick, on, onCheck)
import Html.Attributes exposing (type', checked)
import Html.App as Html
import Http exposing (..)
import Task exposing (..)
import List exposing (..)
import Maybe exposing (..)
import String exposing (..)
import Result exposing (Result, formatError)
import Melody exposing (..)
import AbcPerformance exposing (..)
import Repeats exposing (Repeats, Section, buildRepeatedMelody)
import Abc.ParseTree exposing (..)
import Abc exposing (parse, parseError)

main =
  Html.program
    { init = (init, Cmd.none), update = update, view = view, subscriptions = \_ -> Sub.none }

-- MODEL

type alias Model =
    { performance : Result String (MelodyLine, Repeats) 
    , expandRepeats : Bool
    }

init : Model
init =
  { performance = Err "not started", expandRepeats = False  }

-- UPDATE

type Msg
    = NoOp
    | ExpandRepeats Bool
    | Load String
    | Abc (Result String (MelodyLine, Repeats) )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp -> (model, Cmd.none )

    ExpandRepeats b -> ( { model | expandRepeats = b}, Cmd.none )

    Abc result ->  ( { model | performance = result }, Cmd.none ) 

    Load url -> (model, loadAbc url model.expandRepeats) 

   
mToList : Maybe (List a) -> List a
mToList m = case m of
   Nothing -> []
   Just x -> x



{- load an ABC file -}
loadAbc : String -> Bool -> Cmd Msg
loadAbc url expandRepeats = 
      let settings =  { defaultSettings | desiredResponseType  = Just "text/plain; charset=utf-8" }   
        in
          Http.send settings
                          { verb = "GET"
                          , headers = []
                          , url = url
                          , body = empty
                          } 
          |> Task.toResult
          |> Task.map extractResponse
          |> Task.map (parseLoadedFile expandRepeats)
          |> Task.perform (\_ -> NoOp) Abc

{- extract the true response, concentrating on 200 statuses - assume other statuses are in error
   (usually 404 not found)
-}
extractResponse : Result RawError Response -> Result String Value
extractResponse result =
  case result of
    Ok response -> 
      case response.status of
        200 -> 
          Ok response.value
        _ -> 
          Err (toString (response.status) ++ ": " ++ response.statusText)
    Err e -> Err "unexpected http error"

{- cast a String to an Int -}
toInt : String -> Int
toInt = String.toInt >> Result.toMaybe >> Maybe.withDefault 0

toPerformance : Bool -> Result String AbcTune -> Result String (MelodyLine, Repeats)
toPerformance expandRepeats r = 
   Result.map (melodyFromAbc expandRepeats) r


parseLoadedFile : Bool -> Result String Value -> Result String (MelodyLine, Repeats)
parseLoadedFile expandRepeats r = 
  case r of
    Ok text -> case text of
      Text s -> 
        s 
         |> parse
         |> formatError parseError 
         |> toPerformance expandRepeats
      Blob b -> 
        Err "Blob unsupported"
    Err e -> Err e

-- VIEW

(=>) = (,)

viewPerformanceResult : Result String (MelodyLine, Repeats) -> String
viewPerformanceResult mr = case mr of
      Ok (mel, rpts) -> "OK: " ++ (toString mel) ++ "\nRepeats: " ++ (toString rpts)
      Err errs -> "Fail: " ++ (toString errs)


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
        , input [ type' "checkbox", checked model.expandRepeats, onCheck ExpandRepeats ] []
        , text "expand repeats"
        ]
    , div [  ] [ text ("parse result: " ++ (viewPerformanceResult model.performance)) ]
    ]



