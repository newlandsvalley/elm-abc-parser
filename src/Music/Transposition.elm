module Music.Transposition
  ( 
    keyDistance
  ) where

{-|  Module for score transposition

# Definition

# Functions
@docs keyDistance

WARNING - NOT COMPLETE

-}

import Dict exposing (Dict, fromList, get)
import Maybe exposing (withDefault)
import Result exposing (Result)
import Abc.ParseTree exposing (..)

{- PUBLISHED API -}

{- work out the distance between the keys (target - source) measured in semitones 
   which must be in compaitble modes  
-} 
keyDistance : KeySignature -> KeySignature -> Result String Int
keyDistance target src =
  if (target.mode /= src.mode) then
    Err "incompatible modes"
  else
    Ok (transpositionDistance (target.pitchClass, target.accidental) (src.pitchClass, src.accidental))

{- transpose a note from its source key to its target NOT FINISHED -}
transposeNote : KeySignature -> KeySignature -> AbcNote -> Result String AbcNote
transposeNote targetKey srcKey note =
  if (targetKey.mode /= srcKey.mode) then
    Err "incompatible modes"
  else
    let 
      dist = (transpositionDistance (targetKey.pitchClass, targetKey.accidental) (srcKey.pitchClass, srcKey.accidental))
      srcNum = noteNumber note
  in
    Ok note


{- IMPLEMENTATION -}

{- create a list of pairs which should match every possible
   note pitch  (pitch class and accidental) with its offset into
   its 12-note chromatic scale
-}
noteNumbers : List ((PitchClass, Accidental), Int)
noteNumbers = [ ((C, Flat), 11)
              , ((C, Natural), 0)
              , ((C, Sharp), 1)
              , ((C, DoubleSharp), 2)
              , ((D, DoubleFlat), 0)
              , ((D, Flat), 1)
              , ((D, Natural),2)
              , ((D, Sharp),3)
              , ((D, DoubleSharp),4)
              , ((E, DoubleFlat),2) 
              , ((E, Flat),3) 
              , ((E, Natural),4) 
              , ((E, Sharp),5) 
              , ((E, DoubleSharp),6) 
              , ((F, Flat),4) 
              , ((F, Natural),5) 
              , ((F, Sharp),6) 
              , ((F, DoubleSharp),7) 
              , ((G, DoubleFlat),5)  
              , ((G, Flat),6)  
              , ((G, Natural),7)
              , ((G, Sharp),8)
              , ((G, DoubleSharp),9)
              , ((A, DoubleFlat),7) 
              , ((A, Flat),8) 
              , ((A, Natural),9)
              , ((A, Sharp),10)
              , ((A, DoubleSharp),11)
              , ((B, DoubleFlat),9) 
              , ((B, Flat),10) 
              , ((B, Natural),11)
              , ((B, Sharp),0)
              , ((B, DoubleSharp),1)
            ]

{- note pairs for the black and white notes of a piano,
   designating black notes with the Sharp accidental
-}
sharpNoteNumbers : List ((PitchClass, Accidental), Int)
sharpNoteNumbers =
  let 
    f nn =
      let
        ((pc,a),i) = nn
      in
        ((a == Sharp) && (pc /= E && pc /= B))
          || (a == Natural)
  in
    List.filter f noteNumbers

{- note pairs for the black and white notes of a piano,
   designating black notes with the Flat accidental
-}
flatNoteNumbers : List ((PitchClass, Accidental), Int)
flatNoteNumbers =
  let 
    f nn =
      let
        ((pc,a),i) = nn
      in
        ((a == Flat) && (pc /= F && pc /= C))
          || (a == Natural)
  in
    List.filter f noteNumbers

{- make a note's pitch comparable by translating to a string
   so it can be used in dictionaries
-}
comparableNote : (PitchClass, Accidental) -> String
comparableNote n =
  let
    (pc, acc) = n
    accStr = case acc of
      Natural -> ""
      Sharp -> "#"
      Flat -> "b"
      DoubleSharp -> "##"
      DoubleFlat -> "bb"
  in 
    toString pc ++ accStr

{- noteNumbers with the notes converted to comparable strings for use in dictionaries -}
comparableNoteNumbers :  List (String, Int)
comparableNoteNumbers =
  let 
    f notePair = (comparableNote (fst notePair), (snd notePair))
  in
    List.map f noteNumbers

{- a dictionary of comparable note -> note number -}
chromaticScaleDict : Dict String Int
chromaticScaleDict =
  Dict.fromList comparableNoteNumbers

lookupChromatic : Dict String Int -> String ->  Int
lookupChromatic dict target =
    Dict.get target dict
      |> withDefault 0

{- look up the pitch and return a number in the range 0-11 (0 is C Natural) -}
pitchNumber : (PitchClass, Accidental) -> Int
pitchNumber pa =
  lookupChromatic chromaticScaleDict (comparableNote pa) 

{- look up the note and return the number of its pitch in the range 0-11 (0 is C Natural) -}
noteNumber : AbcNote -> Int
noteNumber n =
  let
    acc = n.accidental
            |> withDefault Natural
  in
   pitchNumber (n.pitchClass, acc)

{- work out the transposition distance (target - source) measured in semitones -} 
transpositionDistance : (PitchClass, Maybe Accidental) -> (PitchClass, Maybe Accidental) -> Int
transpositionDistance target src  =
  let
    (spc, smacc) = src 
    (tpc, tmacc) = target
    sacc = smacc |> withDefault Natural 
    tacc = tmacc |> withDefault Natural 
  in 
   pitchNumber (tpc, tacc) - pitchNumber (spc, sacc)




