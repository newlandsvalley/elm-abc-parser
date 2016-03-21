module Music.Transposition
  ( 
    keyDistance
  , transposeNote
  , transposeNoteBy
  , transposeTo
  ) where

{-|  Experimental Module for tune transposition

You can transpose a parsed ABC tune (in whatever key - perhaps in C) to (say) G# using:

    transposedResult = formatError (\_ -> "parse error") (parse source)
        `andThen` (\tune -> transposeTo ({pitchClass = G, accidental = Just Sharp, mode = Major},[]) tune)


The mode before and after transposition must be identical (that is: you cannot transpose minor to major and so on).
Chord symbols will be lost on transposition.

# Definition

# Functions
@docs keyDistance
    , transposeNote
    , transposeNoteBy
    , transposeTo

WARNING - NOT COMPLETE; NOT FULLY TESTED

-}

{- A parse tree score contains implicit accidentals.  Very often, the source text will not mark them but assume that they
   are implicit in the key signature.  In order for the transposition process to work, all accidentals must be made
   explicit during transposition and then (perhaps) made implicit when wtitten out to text
-}

import Dict exposing (Dict, fromList, get)
import Maybe exposing (withDefault)
import Maybe.Extra exposing (isJust)
import Result exposing (Result)
import Abc.ParseTree exposing (..)
import Music.Notation exposing (isCOrSharpKey, getKeySig, accidentalImplicitInKey)

import Debug exposing (..)

notesInDiatonicScale : Int
notesInDiatonicScale = 12

-- Exposed API

{- work out the distance between the keys (target - source) measured in semitones 
   which must be in compatible modes  
-} 
keyDistance : ModifiedKeySignature -> ModifiedKeySignature -> Result String Int
keyDistance targetmks srcmks =
  let
     target = fst targetmks
     src = fst srcmks
  in
    if (target.mode /= src.mode) then
      Err "incompatible modes"
    else
      Ok (transpositionDistance (target.pitchClass, target.accidental) (src.pitchClass, src.accidental))

{- transpose a note from its source key to its target -}
transposeNote : ModifiedKeySignature -> ModifiedKeySignature -> AbcNote -> Result String AbcNote
transposeNote targetKey srcKey note =
  let
    rdist = keyDistance targetKey srcKey
  in
    case rdist of
      Err e -> Err e
      Ok d  -> Ok (transposeNoteBy targetKey srcKey d note)

{-| transpose a note by the required distance which may be positive or negative -}
transposeNoteBy : ModifiedKeySignature -> ModifiedKeySignature -> Int -> AbcNote -> AbcNote
transposeNoteBy targetKs srcks dist note =
  let
    -- make any implicit accidental explicit in the note to be transposed if it's not marked as an accidental
    inKeyAccidental = accidentalImplicitInKey note srcks
    explicitNote = 
      if (isJust note.accidental) then
        note
      else 
        { note | accidental = inKeyAccidental }
    _ = log "note to transpose" note
    srcNum = log "src num" (noteNumber explicitNote)
    (targetNum, octaveIncrement) = log "note index" (noteIndex srcNum dist)
    (pc, acc) = pitchFromInt (fst targetKs) targetNum
    macc = case acc of
      Natural -> Nothing
      x -> Just x
   in 
    { note | pitchClass = pc, accidental = macc, octave = note.octave + octaveIncrement }

{-| transpose a tune to the target key -}
transposeTo : ModifiedKeySignature -> AbcTune -> Result String AbcTune
transposeTo targetmks t =
  let
    -- get the key signature if there is one, default to C Major
    mks = log "transpose from key" (getKeySig t
           |> withDefault ( {pitchClass = C, accidental = Nothing, mode = Major}, []))
    -- find the distance between the keys
    rdistance = keyDistance targetmks mks
  in
    case rdistance of
      Err e -> Err e
      Ok d -> Ok (transposeTune targetmks mks d t)

-- Implementation
transposeTune : ModifiedKeySignature -> ModifiedKeySignature -> Int -> AbcTune -> AbcTune
transposeTune targetks srcks i t =
  let
    (headers, body) = t
    newHeaders = replaceKeyHeader targetks headers
  in
    (newHeaders, (transposeTuneBody targetks srcks i body))

transposeTuneBody : ModifiedKeySignature -> ModifiedKeySignature -> Int -> TuneBody -> TuneBody
transposeTuneBody targetks srcks i  =
  List.map (transposeBodyPart targetks srcks i)

transposeBodyPart : ModifiedKeySignature -> ModifiedKeySignature -> Int -> BodyPart -> BodyPart
transposeBodyPart targetks srcks i bp =
  case bp of
    Score ms -> Score (transposeMusicList targetks srcks i ms)
    _ -> bp

transposeMusic : ModifiedKeySignature -> ModifiedKeySignature -> Int -> Music -> Music
transposeMusic targetks srcks i m =
  case m of
    Note n -> Note (transposeNoteBy targetks srcks i n)
    BrokenRhythmPair n1 b n2 ->  BrokenRhythmPair (transposeNoteBy targetks srcks i n1) b (transposeNoteBy targetks srcks i n2)
    Tuplet ts ns -> Tuplet ts (transposeNoteList targetks srcks i ns)
    GraceNote b m -> GraceNote b (transposeMusic targetks srcks i m)
    Chord c -> Chord (transposeChord targetks srcks i c)
    NoteSequence ms -> NoteSequence  (transposeMusicList targetks srcks i ms)
    -- we won't attempt to transpose chord symbols - just quietly drop them
    ChordSymbol s -> Ignore   
    _ -> m

transposeMusicList : ModifiedKeySignature -> ModifiedKeySignature -> Int -> List Music -> List Music
transposeMusicList targetks srcks i =
  List.map (transposeMusic targetks srcks i)

transposeNoteList : ModifiedKeySignature -> ModifiedKeySignature -> Int -> List AbcNote -> List AbcNote
transposeNoteList targetks srcks i =
  List.map (transposeNoteBy targetks srcks i)

transposeChord : ModifiedKeySignature -> ModifiedKeySignature -> Int -> AbcChord -> AbcChord
transposeChord targetks srcks i c =
  { c | notes = transposeNoteList targetks srcks i c.notes }



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

{- work out the transposition distance (target - source) measured in semitones -} 
transpositionDistance : (PitchClass, Maybe Accidental) -> (PitchClass, Maybe Accidental) -> Int
transpositionDistance target src  =
  let
    (spc, smacc) = src 
    (tpc, tmacc) = target
    sacc = smacc |> withDefault Natural 
    tacc = tmacc |> withDefault Natural 
    -- _ = log "transpose source" (spc, sacc)
    -- _ = log "transpose target" (tpc, tacc)
  in 
   log "transposition distance" (pitchNumber (tpc, tacc) - pitchNumber (spc, sacc))

{- replace a Key header (if it exists) -}
replaceKeyHeader : ModifiedKeySignature -> TuneHeaders -> TuneHeaders
replaceKeyHeader newmks hs =
  let
    f h = case h of
      Key mks -> False
      _ -> True
    newhs = List.filter f hs
  in
    newhs ++ [Key newmks]




