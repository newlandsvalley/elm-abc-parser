module Player where

import Effects exposing (Effects, task)
import Html exposing (..)
import Html.Events exposing (onClick, on, targetChecked)
import Html.Attributes exposing (type', checked)
import Http exposing (..)
import Task exposing (..)
import List exposing (..)
import Maybe exposing (..)
import String exposing (..)
import Result exposing (Result, formatError)
import Signal exposing (Address)
import Melody exposing (..)
import AbcPerformance exposing (..)
import Repeats exposing (Repeats, Section, buildRepeatedMelody)
import Abc.ParseTree exposing (..)
import Abc exposing (parse, parseError)

-- MODEL

type alias Model =
    { performance : Result String (MelodyLine, Repeats) 
    , expandRepeats : Bool
    }

init : String -> (Model, Effects Action)
init topic =
  ( { performance = Err "not started", expandRepeats = False  }
  , Effects.none
  )

-- UPDATE

type Action
    = NoOp
    | ExpandRepeats Bool
    | Load String
    | Abc (Result String (MelodyLine, Repeats) )

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none )

    ExpandRepeats b -> ( { model | expandRepeats = b}, Effects.none )

    Abc result ->  ( { model | performance = result }, Effects.none ) 

    Load url -> (model, loadAbc url model.expandRepeats) 

   
mToList : Maybe (List a) -> List a
mToList m = case m of
   Nothing -> []
   Just x -> x



{- load an ABC file -}
loadAbc : String -> Bool -> Effects Action
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
          |> Task.map Abc
          |> Effects.task

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


view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ button [ onClick address (Load "abc/lillasystern.abc") ] [ text "lillasystern" ]
    , button [ onClick address (Load "abc/PolskaRattvik.abc") ] [ text "Rattvik polska" ]
    , button [ onClick address (Load "abc/justnotes.abc") ] [ text "just notes" ]
    , button [ onClick address (Load "abc/twobars.abc") ] [ text "two bars" ]
    , button [ onClick address (Load "abc/baraccidental.abc") ] [ text "bar accidental" ]
    , button [ onClick address (Load "abc/tie.abc") ] [ text "tie" ]
    , button [ onClick address (Load "abc/chord.abc") ] [ text "chord" ]
    , button [ onClick address (Load "abc/variantending.abc") ] [ text "variant ending" ]
    , button [ onClick address (Load "abc/repeat1.abc") ] [ text "repeat 1" ]
    , button [ onClick address (Load "abc/repeat2.abc") ] [ text "repeat 2" ]
    , button [ onClick address (Load "abc/repeat3.abc") ] [ text "repeat 3" ]
    , button [ onClick address (Load "abc/repeat4.abc") ] [ text "repeat 4" ]
    , button [ onClick address (Load "abc/repeat5.abc") ] [ text "repeat 5" ]
    , button [ onClick address (Load "abc/repeat6.abc") ] [ text "repeat 6" ]
    , div [  ] (checkbox address model.expandRepeats ExpandRepeats "expand repeats")
    , div [  ] [ text ("parse result: " ++ (viewPerformanceResult model.performance)) ]
    ]

checkbox : Address Action -> Bool -> (Bool -> Action) -> String -> List Html
checkbox address isChecked tag name =
  [ input
      [ type' "checkbox"
      , checked isChecked
      , on "change" targetChecked (Signal.message address << tag)
      ]
      []
  , text name
  , br [] []
  ]


-- INPUTS


signals : List (Signal Action)
signals = []


