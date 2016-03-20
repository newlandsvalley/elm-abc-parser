module Music.Transposition
  ( 
    keyDistance
  , transposeNote
  ) where

{-|  Module for score transposition

# Definition

# Functions
@docs keyDistance
    , transposeNote

WARNING - NOT COMPLETE

-}

import Dict exposing (Dict, fromList, get)
import Maybe exposing (withDefault)
import Result exposing (Result)
import Abc.ParseTree exposing (..)
import Music.Notation exposing (isCOrSharpKey)

import Debug exposing (..)

notesInDiatonicScale : Int
notesInDiatonicScale = 12

{- EXPOSED API -}

{- work out the distance between the keys (target - source) measured in semitones 
   which must be in compatible modes  
-} 
keyDistance : KeySignature -> KeySignature -> Result String Int
keyDistance target src =
  if (target.mode /= src.mode) then
    Err "incompatible modes"
  else
    Ok (transpositionDistance (target.pitchClass, target.accidental) (src.pitchClass, src.accidental))

{- transpose a note from its source key to its target -}
transposeNote : KeySignature -> KeySignature -> AbcNote -> Result String AbcNote
transposeNote targetKey srcKey note =
  if (targetKey.mode /= srcKey.mode) then
    Err "incompatible modes"
  else
    let 
      srcNum = log "note num" (noteNumber note)
      dist = log "distance" (transpositionDistance (targetKey.pitchClass, targetKey.accidental) (srcKey.pitchClass, srcKey.accidental))
      (targetNum, octaveIncrement) = noteIndex srcNum dist
      (pc, acc) = pitchFromInt targetKey targetNum
      macc = case acc of
        Natural -> Nothing
        x -> Just x
  in
    Ok { note | pitchClass = pc, accidental = macc, octave = note.octave + octaveIncrement }

{- inspect the current note index and the amount it is to be incremented by.
   produce a new note index in the range (0 <= n < notesInDiatonicScale)
   and associate with this a number (-1,0,1) which indicates an increment to the octave
-}
noteIndex : Int -> Int -> (Int, Int)
noteIndex from increment =
  let
    to = (from + increment)
  in 
    if to < 0 then
      ((notesInDiatonicScale + to), -1)
    else if (to >= notesInDiatonicScale) then
      ((to - notesInDiatonicScale), 1)
    else
      (to, 0)


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

{- given a key signature and an integer (0 <= n < notesInDiatonicScale)
   return the pitch of the note within that signature
-}
pitchFromInt : KeySignature -> Int -> (PitchClass, Accidental)
pitchFromInt ks i =
  let
    _ = log "pitchFromInt" i
    dict =
      if (isCOrSharpKey ks) then
        sharpNotedNumbers
      else
        flatNotedNumbers
  in
    Dict.get i dict
      |> withDefault (C, Natural)

{- the inverted lookup for sharp chromatic scales.  This dictionaary
   allows you to enter a number (0 <= n < notesInDiatonicScale) and return
   a (pitchClass, Accidental) pair which is the note's pitch
-}
sharpNotedNumbers : Dict Int (PitchClass, Accidental)
sharpNotedNumbers =
  let
    invert (a, b) = (b, a)
  in
    List.map invert sharpNoteNumbers
      |> Dict.fromList 

{- the inverted lookup for flat chromatic scales.  This dictionaary
   allows you to enter a number (0 <= n < notesInDiatonicScale) and return 
   a (pitchClass, Accidental) pair which is the note's pitch
-}
flatNotedNumbers : Dict Int (PitchClass, Accidental)
flatNotedNumbers =
  let
    invert (a, b) = (b, a)
  in
    List.map invert flatNoteNumbers
      |> Dict.fromList 

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

{- look up the pitch and return a number in the range 0 <= n < notesInDiatonicScale  (0 is C Natural) -}
pitchNumber : (PitchClass, Accidental) -> Int
pitchNumber pa =
  lookupChromatic chromaticScaleDict (comparableNote pa) 

{- look up the note and return the number of its pitch in the range 0 <= n < notesInDiatonicScale (0 is C Natural) -}
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




