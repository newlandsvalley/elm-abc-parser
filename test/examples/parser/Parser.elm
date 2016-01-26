module Parser where

import Effects exposing (Effects, task)
import Html exposing (..)
import Html.Events exposing (onClick)
import Http exposing (..)
import Task exposing (..)
import List exposing (..)
import Maybe exposing (..)
import String exposing (..)
import Result exposing (Result)
import AbcTuneParser exposing (..)

-- MODEL

type alias Model =
    { transcription : Result String AbcTune
    }

init : String -> (Model, Effects Action)
init topic =
  ( { transcription = Err "not started"  }
  , Effects.none
  )

-- UPDATE

type Action
    = NoOp
    | Load String
    | Abc (Result String AbcTune )

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none )

    Abc result ->  ( { transcription = result }, Effects.none ) 

    Load url -> (model, loadAbc url) 
  


   
mToList : Maybe (List a) -> List a
mToList m = case m of
   Nothing -> []
   Just x -> x



{- load a MIDI file -}
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


parseLoadedFile : Result String Value -> Result String AbcTune
parseLoadedFile r = 
  case r of
    Ok text -> case text of
      Text s -> 
        s |> parse
      Blob b -> 
        Err "Blob unsupported"
    Err e -> Err e

-- VIEW

viewParseResult : Result String AbcTune -> String
viewParseResult mr = 
   case mr of
      Ok res -> 
         "OK: " ++ (toString res)
      Err errs -> 
         "Fail: " ++ (toString errs)


view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ 
      button [ onClick address (Load "abc/justnotes.abc") ] [ text "just notes" ]
    , button [ onClick address (Load "abc/lillasystern.abc") ] [ text "lillasystern" ]
    , button [ onClick address (Load "abc/PolskaRattvik.abc") ] [ text "Rattvik polska" ]
    , button [ onClick address (Load "abc/LasseiLyby.abc") ] [ text "Lasse i Lyby" ]
    , button [ onClick address (Load "abc/ChordSymbols.abc") ] [ text "cord symbols sample" ]
    , button [ onClick address (Load "abc/Chords.abc") ] [ text "cord sample" ]
    , button [ onClick address (Load "abc/inline.abc") ] [ text "inline info sample" ]
    , button [ onClick address (Load "abc/complextuplet.abc") ] [ text "tuplet sample" ]
    , button [ onClick address (Load "abc/slurs.abc") ] [ text "slurs sample" ]
    , button [ onClick address (Load "abc/grace.abc") ] [ text "grace note sample" ]
    , button [ onClick address (Load "abc/coda.abc") ] [ text "decoration (coda)" ]
    , button [ onClick address (Load "abc/staccato.abc") ] [ text "decoration (staccato)" ]
    , div [  ] [ text ("parse result: " ++ (viewParseResult model.transcription)) ]
    ]

-- INPUTS


signals : List (Signal Action)
signals = []


