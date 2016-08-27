module Parser exposing (..)

import Html exposing (..)
import Html.Events exposing (onCheck, onClick, on)
import Html.Attributes exposing (type', checked, rows, cols)
import Html.App as Html
import Http exposing (..)
import Task exposing (..)
import List exposing (..)
import Maybe exposing (..)
import String exposing (..)
import Result exposing (Result, formatError)
import Abc exposing (..)
import Abc.ParseTree exposing (..)
import Abc.Canonical exposing (..)

main =
  Html.program
    { init = (init, Cmd.none), update = update, view = view, subscriptions = \_ -> Sub.none }

-- MODEL
type alias Model =
    {  roundTrip : Bool
    ,  transcription : Result String AbcTune
    }

init : Model
init =
  { roundTrip = False, transcription = Err "not started"  }

-- UPDATE

type Msg
    = NoOp
    | RoundTrip Bool
    | Load String
    | Abc (Result String AbcTune )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp -> (model, Cmd.none )

    RoundTrip b -> ( { model | roundTrip = b}, Cmd.none )

    Abc result ->  ( { model | transcription = result }, Cmd.none ) 

    Load url -> (model, loadAbc url)   


   
mToList : Maybe (List a) -> List a
mToList m = case m of
   Nothing -> []
   Just x -> x



{- load an ABC file -}
loadAbc : String -> Cmd Msg
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

view : Model -> Html Msg
view model =
  div []    
    [ 
    text "parse:"
    ,  ul []      
      [
        li [] [
              button [ onClick (Load "abc/justnotes.abc") ] [ text "just notes" ]
              ]
      , li [] [
              button [ onClick (Load "abc/octaves.abc") ] [ text "octaves" ]
              ]
      , li [] [
              button [ onClick (Load "abc/lillasystern.abc") ] [ text "lillasystern" ]
              ]
      , li [] [
              button [ onClick (Load "abc/PolskaRattvik.abc") ] [ text "Rattvik polska" ]
              ]
      , li [] [
              button [ onClick (Load "abc/LasseiLyby.abc") ] [ text "Lasse i Lyby" ]
              ]
      , li [] [
              button [ onClick (Load "abc/ChordSymbols.abc") ] [ text "cord symbols sample" ]
              ]
      , li [] [
              button [ onClick (Load "abc/Chords.abc") ] [ text "cord sample" ]
              ]
      , li [] [
              button [ onClick (Load "abc/chord.abc") ] [ text "one cord sample" ]
              ]
      , li [] [
              button [ onClick (Load "abc/inline.abc") ] [ text "inline info sample" ]
              ]
      , li [] [
              button [ onClick (Load "abc/complextuplet.abc") ] [ text "tuplet sample" ]
              ]
      , li [] [
              button [ onClick (Load "abc/slurs.abc") ] [ text "slurs sample" ]
              ]
      , li [] [
              button [ onClick (Load "abc/grace.abc") ] [ text "grace note sample" ]
              ]
      , li [] [
              button [ onClick (Load "abc/coda.abc") ] [ text "decoration (coda)" ]
              ]
      , li [] [
              button [ onClick (Load "abc/staccato.abc") ] [ text "decoration (staccato)" ]
              ]
      , li [] [
              button [ onClick (Load "abc/comment.abc") ] [ text "comment sample" ]
              ]
      , li [] [
              button [ onClick (Load "abc/wordsaligned.abc") ] [ text "words aligned" ]
              ]
      , li [] [
              button [ onClick (Load "abc/keysig.abc") ] [ text "key signature" ]
              ]
      , li [] [
              button [ onClick (Load "abc/tie.abc") ] [ text "tie" ]
              ]
      , li [] [
              button [ onClick (Load "abc/fractionalnote.abc") ] [ text "fractional note" ]
              ]
      , li [] [
              button [ onClick (Load "abc/badinput.abc") ] [ text "bad input" ]
              ]
      , li [] [
              button [ onClick (Load "abc/badinput1.abc") ] [ text "bad input 1" ]
              ]
      , li [] [
              button [ onClick (Load "abc/badheader.abc") ] [ text "bad header" ]
              ]
      ]
    , label []
        [ br [] []
        , input [ type' "checkbox", checked model.roundTrip, onCheck RoundTrip ] []
        , text "round trip"
        ]
    , div [  ] [ 
         textarea textAreaStyle  [text (viewResult model) ]
         ]
    ]

-- STYLE

textAreaStyle : List (Attribute Msg)
textAreaStyle =
  [ rows 40
  , cols 180
  ]



