module Player where

import Effects exposing (Effects, task)
import Html exposing (..)
import Html.Events exposing (onClick)
import Http exposing (..)
import Task exposing (..)
import List exposing (..)
import Maybe exposing (..)
import String exposing (..)
import Result exposing (Result, formatError)
import AbcPerformance exposing (..)
import Abc.ParseTree exposing (..)
import Abc exposing (parse, parseError)

-- MODEL

type alias Model =
    { performance : Result String MelodyLine
    }

init : String -> (Model, Effects Action)
init topic =
  ( { performance = Err "not started"  }
  , Effects.none
  )

-- UPDATE

type Action
    = NoOp
    | Load String
    | Abc (Result String MelodyLine )

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none )

    Abc result ->  ( { performance = result }, Effects.none ) 

    Load url -> (model, loadAbc url) 

   
mToList : Maybe (List a) -> List a
mToList m = case m of
   Nothing -> []
   Just x -> x



{- load an ABC file -}
loadAbc : String -> Effects Action
loadAbc url = 
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
          |> Task.map parseLoadedFile
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

toPerformance : Result String AbcTune -> Result String MelodyLine
toPerformance r = Result.map fromAbc r

parseLoadedFile : Result String Value -> Result String MelodyLine
parseLoadedFile r = 
  case r of
    Ok text -> case text of
      Text s -> 
        s 
         |> parse
         |> formatError parseError 
         |> toPerformance
      Blob b -> 
        Err "Blob unsupported"
    Err e -> Err e

-- VIEW

(=>) = (,)

viewPerformanceResult : Result String MelodyLine -> String
viewPerformanceResult mr = case mr of
      Ok res -> "OK: " ++ (toString res)
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
    , div [  ] [ text ("parse result: " ++ (viewPerformanceResult model.performance)) ]
    ]

-- INPUTS


signals : List (Signal Action)
signals = []


