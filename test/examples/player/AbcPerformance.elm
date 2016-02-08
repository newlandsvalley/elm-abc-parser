module AbcPerformance (  NoteEvent
                       , MelodyLine
                       , fromAbc
                       ) where

{-|  conversion of a ABC Tune parse tree to a performance

# Definition

# Data Types
@docs NoteEvent, MelodyLine

# Functions
@docs fromAbc

-}

import Abc.ParseTree exposing (..)
import Music.Notation exposing (..)
import String exposing (fromChar, toUpper)
import Ratio exposing (Rational, over, fromInt, toFloat, add)

type alias AccumulatedTime = Float

{-| a Note Event (no pitch class implies a rest) -}    
type alias SingleNote = (NoteTime, MidiPitch, Maybe PitchClass)

type NoteEvent =
     ANote SingleNote
   | AChord (List SingleNote)

type alias MelodyLine = List NoteEvent

type alias TranslationState = 
   { keySignature : KeySignature
   , tempo : AbcTempo
   , tempoModifier : Float
   }

-- default to 1/4=120
defaultTempo : AbcTempo
defaultTempo = 
  {  tempoNoteLength = over 1 4
  ,  bpm = 120
  ,  unitNoteLength = over 1 8
  }

-- default to C Major
defaultKey : KeySignature
defaultKey = 
  { pitchClass = C
  , accidental = Nothing
  , mode = Major
  } 

-- get the tempo from the tune header
{-
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
-}

{- update the state of the player when we come across a header (either at the start or inline)
   which affects the tune tempo or the pitch of a note (i.e. they key)
-}
updateState : Header -> (MelodyLine, TranslationState) -> (MelodyLine, TranslationState)
updateState h acc =
  let 
    (melody, state) = acc
    tempo = state.tempo
  in case h of
    UnitNoteLength d ->
      (melody, { state | tempo = { tempo | unitNoteLength = d }} )
    Tempo t ->
      let 
        tnl = List.foldl Ratio.add (fromInt 0) t.noteLengths
      in
       (melody, { state | tempo = { tempo | tempoNoteLength = tnl, bpm = t.bpm }} )
    Key k ->
       (melody, { state | keySignature = k} )
    _ -> acc       

{- translate a sequence of notes as found in chords (parallel) or tuplets (sequential) -}
translateNoteSequence : Bool -> TranslationState -> List AbcNote -> MelodyLine
translateNoteSequence isSeq state notes =
  let
    f abc = 
      let 
        duration = (noteDuration state.tempo abc.duration) * state.tempoModifier
      in
        (duration, toMidiPitch abc state.keySignature, Just abc.pitchClass)
  in
    if isSeq then 
       List.map f notes
         |> List.map (\a -> ANote a)
    else 
       [AChord (List.map f notes)]

{- translate a pair of notes, each working under a separate state -}
translateNotePair : AbcNote -> TranslationState -> AbcNote -> TranslationState -> MelodyLine -> MelodyLine
translateNotePair n1 s1 n2 s2 ml =
  let      
    duration1 = (noteDuration s1.tempo n1.duration) * s1.tempoModifier
    duration2 = (noteDuration s2.tempo n2.duration) * s2.tempoModifier
    note1 = ANote (duration1, toMidiPitch n1 s1.keySignature, Just n1.pitchClass) 
    note2 = ANote (duration2, toMidiPitch n2 s2.keySignature, Just n2.pitchClass) 
  in
    (note1 :: note2 :: ml)

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
          duration = (noteDuration state.tempo abc.duration) * state.tempoModifier
          line = ANote (duration, toMidiPitch abc state.keySignature, Just abc.pitchClass) :: melodyLine
        in
          (line, state)
      Rest r -> 
        let 
          duration = (noteDuration state.tempo r) * state.tempoModifier
          line = ANote (duration, 0, Nothing) :: melodyLine 
        in        
          (line, state)
      Tuplet signature notes ->
        let 
          (p,q,r) = signature
          newState = { state | tempoModifier = ( Basics.toFloat q / Basics.toFloat p) }
          tuplet = translateNoteSequence True newState notes
          line = List.append tuplet melodyLine         
        in 
          (line, state)
      BrokenRhythmPair n1 b n2 ->     
        case b of 
          LeftArrow i ->
            let
              leftState =  { state | tempoModifier = ( 1 - dotFactor i) }
              rightState =  { state | tempoModifier = ( 1 + dotFactor i) }
            in
              (translateNotePair n1 leftState n2 rightState melodyLine, state)
          RightArrow i ->
            let
              leftState =  { state | tempoModifier = ( 1 + dotFactor i) }
              rightState =  { state | tempoModifier = ( 1 - dotFactor i) }
            in
              (translateNotePair n1 leftState n2 rightState melodyLine, state)
      Chord notes ->
        let 
          chord = translateNoteSequence False state notes
          line = List.append chord melodyLine         
        in 
          (line, state)
      _ -> acc

-- translate an entire melody line from the tune body (up to an end of line)
toMelodyLine : MusicLine -> (MelodyLine, TranslationState) -> (MelodyLine, TranslationState)
toMelodyLine ml state =
  -- List.foldl translateMusic state ml
  List.foldr translateMusic state ml
 
{- translate an AbcTune to a more playable melody line
   which is a list of notes (or rests) and their durations
-}
fromAbc : AbcTune -> MelodyLine
fromAbc tune =   
  let
    -- set a default state for case where there are no tune headers
    defaultState = ([], { keySignature = defaultKey, tempo = defaultTempo, tempoModifier = 1.0})
    -- update this from the header state if we have any headers
    headerState = List.foldl updateState defaultState (fst tune)
    f bp acc = case bp of
      -- process a line from the melody using the current state
      Score musicLine continuation -> 
        let 
          (existingLine, state) = acc
          (newLine, newState) = toMelodyLine musicLine acc
        in
          (newLine, state)
      -- update the state if we have an inline header
      BodyInfo header -> 
        updateState header acc
   in 
     {-
     List.foldl f headerState (snd tune)
       |> fst
       |> List.reverse
     -}    
     List.foldr f headerState (snd tune)
       |> fst




