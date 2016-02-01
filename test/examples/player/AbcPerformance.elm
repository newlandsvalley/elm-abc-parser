module AbcPerformance (  NoteEvent
                       , AbcTempo
                       , AbcPerformance
                       , fromAbc
                       ) where

{-|  conversion of a ABC Tune parse tree to a performance

# Definition

# Data Types
@docs NoteEvent, AbcTempo, AbcPerformance

# Functions
@docs fromAbc

-}

import Abc.ParseTree exposing (..)
import String exposing (fromChar, toUpper)
import Maybe exposing (withDefault)
import Ratio exposing (Rational, over, fromInt, toFloat, add)
import Dict exposing (Dict, fromList, get)

type alias AccumulatedTime = Float
type alias NoteDuration = Float
type alias MidiPitch = Int


{-| a Note Event -}    
type alias NoteEvent = (NoteDuration, MidiPitch, Char)

type alias MelodyLine = List NoteEvent

{-| AbcPerformance -}    
type alias AbcPerformance = List MelodyLine
 

{-| Abc Performance -}
type alias AbcTempo = 
    { tempoNoteLength : Rational
    , bpm : Int
    , unitNoteLength : Rational
    }

type alias TranslationState = 
   { tempo : AbcTempo
   , keySig : String
   }

{- lookup for providing offsets from C in a chromatic scale -}
chromaticScale : Dict String Int
chromaticScale =
  Dict.fromList
    [ ("C", 0), 
      ("C#", 1),
      ("Db", 1),
      ("D", 2),
      ("D#", 3),
      ("Eb", 3),
      ("E", 4),
      ("F", 5),
      ("F#", 6),
      ("Gb", 6),
      ("G", 7),
      ("G#", 8),
      ("Ab", 8),
      ("A", 9),
      ("A#",10),
      ("Bb",10),
      ("B", 11)
     ]


{- convert an AbcNote (pich class and accidental) to a pitch offset in a chromatic scale -}
midiPitchOffset : AbcNote -> Int
midiPitchOffset n =
  let 
    f a = case a of
      Sharp -> "#"
      Flat -> "b"
      _ -> ""
    accidental = withDefault "" (Maybe.map f n.accidental)
    pattern = (String.fromChar n.pitchClass
                |> toUpper) ++ accidental
  in
    withDefault 0 (Dict.get pattern chromaticScale)

{- convert an ABC note pitch to a MIDI pitch -}
toMidiPitch : AbcNote -> MidiPitch
toMidiPitch n =
  (n.octave * 12) + midiPitchOffset n

defaultTempo : AbcTempo
defaultTempo = 
  {  tempoNoteLength = over 1 4
  ,  bpm = 120
  ,  unitNoteLength = over 1 8
  }

getHeaderTempo : AbcTempo -> TuneHeaders -> AbcTempo
getHeaderTempo a =
  let
    f h acc = 
      case h of
        UnitNoteLength d ->
           { acc | unitNoteLength = d }
        Tempo t ->
          let 
            tnl = List.foldl Ratio.add (fromInt 0) t.noteLengths
          in
           { acc | tempoNoteLength = tnl, bpm = t.bpm }
        _ -> acc       
  in
    List.foldr f a

noteDuration : AbcTempo -> Rational -> NoteDuration
noteDuration t n = 
   (60.0 * (Ratio.toFloat t.unitNoteLength)) / 
    ((Ratio.toFloat t.tempoNoteLength) * (Basics.toFloat t.bpm)) * 
     (Ratio.toFloat n)

{- not at all complete - translate a Music item from the parse tree to a playable note
   (note duration and MIDI pitch) 
-}
translateMusic : Music -> (MelodyLine, TranslationState) -> (MelodyLine, TranslationState)
translateMusic m acc =
  let 
    (melodyLine, state) = acc
  in
    case m of
      Note abc -> 
        let 
          duration = noteDuration state.tempo abc.duration
          line = (duration, toMidiPitch abc, abc.pitchClass) :: melodyLine
        in
          (line, state)
      Rest r -> 
        let 
          duration = noteDuration state.tempo r
          line = (duration, 0, 'z') :: melodyLine 
        in        
          (line, state)
      _ -> acc

toMelodyLine : TranslationState -> MusicLine -> MelodyLine
toMelodyLine state ml =
  let
    (melodyLine, state) = List.foldr translateMusic ([], state) ml
  in
    melodyLine

{- translate an AbcTune to a more playable AbcPerformance 
   which is a list of lines of music consisting
   of just notes (or rests) and their durations
-}
fromAbc : AbcTune -> AbcPerformance
fromAbc tune =   
  let
    acc = { tempo = defaultTempo, keySig = "C" }
    f bp = case bp of
      Score musicLine continuation ->
         toMelodyLine acc musicLine
      _ ->
         []
   in 
     List.map f (snd tune)


{- keep a running total of accumulated ticks -}
{-
accum : MidiMessage -> List MidiMessage -> List MidiMessage 
accum nxt acc = let at = case acc of 
                      [] -> 0
                      x :: xs -> fst x
                    nt = fst nxt
                    nv = snd nxt
                 in
                    (at + nt, nv) :: acc
-}

{-| accumulate the timings and leave only Tempo and NoteOn messages -}
{-
accumulateTimes : Track -> Track
accumulateTimes = filterEvents << List.reverse << List.foldl accum [] 
-}


