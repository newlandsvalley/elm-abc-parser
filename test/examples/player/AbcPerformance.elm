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
import Music.Notation exposing (accidentalImplicitInKey)
import String exposing (fromChar, toUpper)
import Maybe exposing (withDefault, oneOf)
import Ratio exposing (Rational, over, fromInt, toFloat, add)
import Dict exposing (Dict, fromList, get)

type alias AccumulatedTime = Float
type alias NoteDuration = Float
type alias MidiPitch = Int

{-| a Note Event (no pitch class implies a rest) -}    
type alias SingleNote = (NoteDuration, MidiPitch, Maybe PitchClass)

type NoteEvent =
     ANote SingleNote
   | AChord (List SingleNote)

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
   { keySignature : KeySignature
   , tempo : AbcTempo
   , tempoModifier : Float
   }

{- lookup for providing offsets from C in a chromatic scale 
   we have to translate a KeyClass to a string because otherwise
   it can't be used as a Dict key -}
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
midiPitchOffset : AbcNote -> KeySignature -> Int
midiPitchOffset n ks =
  let 
    inKeyAccidental = accidentalImplicitInKey n ks
    -- look first for an explicit then for an implicit accidental attached to this key class
    maybeAccidental = oneOf [n.accidental, inKeyAccidental]
    f a = case a of
      Sharp -> "#"
      Flat -> "b"
      _ -> ""
    accidental = withDefault "" (Maybe.map f maybeAccidental)
    pattern = (toString n.pitchClass) ++ accidental
  in
    withDefault 0 (Dict.get pattern chromaticScale)

{- convert an ABC note pitch to a MIDI pitch -}
toMidiPitch : AbcNote -> KeySignature -> MidiPitch
toMidiPitch n ks =
  (n.octave * 12) + midiPitchOffset n ks

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

-- translate a tempo and unit note length to a real worls note duration
noteDuration : AbcTempo -> Rational -> NoteDuration
noteDuration t n = 
   (60.0 * (Ratio.toFloat t.unitNoteLength)) / 
    ((Ratio.toFloat t.tempoNoteLength) * (Basics.toFloat t.bpm)) * 
     (Ratio.toFloat n)

{- translate a sequence of ABC notes which can be done either in series
   (i.e. the Melody Line is a note sequence] or in parallel (i.e. the
   melody line contains a single chord
-} 
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
      Chord notes ->
        let 
          chord = translateNoteSequence False state notes
          line = List.append chord melodyLine         
        in 
          (line, state)
      _ -> acc

-- translate an entire melody line from the tune body (up to an end of line)
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
    acc = { keySignature = defaultKey, tempo = defaultTempo, tempoModifier = 1.0}
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


