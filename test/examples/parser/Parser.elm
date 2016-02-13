module Parser where

import Effects exposing (Effects, task)
import Html exposing (..)
import Html.Events exposing (onClick, on, targetChecked)
import Html.Attributes exposing (type', checked, rows, cols)
import Http exposing (..)
import Task exposing (..)
import List exposing (..)
import Maybe exposing (..)
import String exposing (..)
import Result exposing (Result, formatError)
import Signal exposing (Address)
import Abc exposing (..)
import Abc.ParseTree exposing (..)
import Abc.Canonical exposing (..)

-- MODEL
type alias Model =
    {  roundTrip : Bool
    ,  transcription : Result String AbcTune
    }

init : String -> (Model, Effects Action)
init topic =
  ( { roundTrip = False, transcription = Err "not started"  }
  , Effects.none
  )

-- UPDATE

type Action
    = NoOp
    | RoundTrip Bool
    | Load String
    | Abc (Result String AbcTune )

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp -> (model, Effects.none )

    RoundTrip b -> ( { model | roundTrip = b}, Effects.none )

    Abc result ->  ( { model | transcription = result }, Effects.none ) 

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


parseLoadedFile : Result String Value -> Result String AbcTune
parseLoadedFile r = 
  case r of
    Ok text -> case text of
      Text s -> 
        s 
          |> parse
          |> formatError parseError
      Blob b -> 
        Err "Blob unsupported"
    Err e -> Err e

-- VIEW

viewAbc : Result String String -> String
viewAbc ra = 
   case ra of
      Ok res -> 
         res
      Err errs -> 
         "Fail: " ++ (toString errs)

viewParseResult : Result String AbcTune -> String
viewParseResult ra = 
   case ra of
      Ok res -> 
         "OK: " ++ (toString res)
      Err errs -> 
         "Fail: " ++ (toString errs)

viewResult : Model -> String
viewResult model =
  if (model.roundTrip) then
    viewAbc (model.transcription |> Abc.Canonical.fromResult)
  else 
    viewParseResult model.transcription

view : Signal.Address Action -> Model -> Html
view address model =
  div []    
    [ 
    text "parse:"
    ,  ul []      
      [
        li [] [
              button [ onClick address (Load "abc/justnotes.abc") ] [ text "just notes" ]
              ]
      , li [] [
              button [ onClick address (Load "abc/octaves.abc") ] [ text "octaves" ]
              ]
      , li [] [
              button [ onClick address (Load "abc/lillasystern.abc") ] [ text "lillasystern" ]
              ]
      , li [] [
              button [ onClick address (Load "abc/PolskaRattvik.abc") ] [ text "Rattvik polska" ]
              ]
      , li [] [
              button [ onClick address (Load "abc/LasseiLyby.abc") ] [ text "Lasse i Lyby" ]
              ]
      , li [] [
              button [ onClick address (Load "abc/ChordSymbols.abc") ] [ text "cord symbols sample" ]
              ]
      , li [] [
              button [ onClick address (Load "abc/Chords.abc") ] [ text "cord sample" ]
              ]
      , li [] [
              button [ onClick address (Load "abc/inline.abc") ] [ text "inline info sample" ]
              ]
      , li [] [
              button [ onClick address (Load "abc/complextuplet.abc") ] [ text "tuplet sample" ]
              ]
      , li [] [
              button [ onClick address (Load "abc/slurs.abc") ] [ text "slurs sample" ]
              ]
      , li [] [
              button [ onClick address (Load "abc/grace.abc") ] [ text "grace note sample" ]
              ]
      , li [] [
              button [ onClick address (Load "abc/coda.abc") ] [ text "decoration (coda)" ]
              ]
      , li [] [
              button [ onClick address (Load "abc/staccato.abc") ] [ text "decoration (staccato)" ]
              ]
      , li [] [
              button [ onClick address (Load "abc/comment.abc") ] [ text "comment sample" ]
              ]
      , li [] [
              button [ onClick address (Load "abc/wordsaligned.abc") ] [ text "words aligned" ]
              ]
      , li [] [
              button [ onClick address (Load "abc/badinput.abc") ] [ text "bad input" ]
              ]
      ]
    , div [  ] (checkbox address model.roundTrip RoundTrip "round trip")
    , div [  ] [ 
         textarea textAreaStyle  [text (viewResult model) ]
         ]
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

-- STYLE

textAreaStyle : List Attribute
textAreaStyle =
  [ rows 40
  , cols 180
  ]

-- INPUTS


signals : List (Signal Action)
signals = []


