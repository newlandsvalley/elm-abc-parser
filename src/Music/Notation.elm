module Music.Notation
  ( KeyClass
  , MidiPitch
  , AbcTempo
  , NoteTime
  , keySet
  , scale
  , accidentalImplicitInKey
  , dotFactor
  , toMidiPitch
  , noteDuration
  ) where

{-|  Helper functions for making more musical sense of the parse tree

# Definition

# Data Types
@docs KeyClass, MidiPitch, AbcTempo, NoteTime

# Functions
@docs keySet, scale, accidentalImplicitInKey, dotFactor, toMidiPitch, noteDuration

-}

import List.Extra exposing (getAt, splitAt, elemIndex, tails)
import List exposing (member)
import Maybe exposing (withDefault, oneOf)
import String exposing (contains, endsWith, fromChar)
import Dict exposing (Dict, fromList, get)
import Abc.ParseTree exposing (Mode (..), Accidental (..), KeySignature, PitchClass (..), AbcNote)
import Ratio exposing (Rational, over, fromInt, toFloat, add)

{-| a complete pitch class (the white note and the accidental) -}
type alias KeyClass = (PitchClass, Maybe Accidental)

type alias ChromaticScale = List KeyClass

type alias Scale = List KeyClass

{- the set of accidentals in a key signature -}
type alias KeySet = List KeyClass

type alias Intervals = List Int

{-| the pitch of a note expressed as a MIDI interval -}
type alias MidiPitch = Int

{-| the time taken when a note is played before the next note -}
type alias NoteTime = Float

{-| ABC header information defining tempo -}
type alias AbcTempo = 
    { tempoNoteLength : Rational
    , bpm : Int
    , unitNoteLength : Rational
    }


-- EXPORTED FUNCTIONS
    
{-| return the set of keys (pitch classes) that comprise the key signature -}
keySet : KeySignature -> KeySet
keySet ks =  
  scale ks
     |> List.filter accidentalKey

{-| return the set of keys (pitche classes) that comprise a complete 11-note scale -}
scale : KeySignature -> Scale
scale ks =
  let 
    target = (ks.pitchClass, ks.accidental)
  in
    case ks.mode of 
      Major -> 
        majorScale target
      Ionian -> 
        majorScale target
      m ->
        modalScale target ks.mode

{-| return an accidental if it is implicitly there in the key signature 
    attached to the pitch class of the note -}
accidentalImplicitInKey : AbcNote -> KeySignature -> Maybe Accidental
accidentalImplicitInKey n ks  =
  let
    keys = keySet ks
    sharpTarget = (n.pitchClass, Just Sharp)
    flatTarget = (n.pitchClass, Just Flat)
  in
    if (member sharpTarget keys) then
      Just Sharp
    else if  (member flatTarget keys) then
      Just Flat
    else
      Nothing

{-| the amount by which you increase the duration of a (multiply) dotted note 
   i.e. duration of a note dotted by x is multiplied by:
      1 + dotFactor x
   and of one symmetrically reduced is
      1 - dotfactor x
-}
dotFactor : Int -> Float
dotFactor i =
  case i of 
    1 -> 0.5 
    2 -> 0.75 
    3 -> 0.875 
    _ -> 0

{-| convert an ABC note pitch to a MIDI pitch -}
toMidiPitch : AbcNote -> KeySignature -> MidiPitch
toMidiPitch n ks =
  (n.octave * 12) + midiPitchOffset n ks

{-| translate a tempo and unit note length to a real world note duration -}
noteDuration : AbcTempo -> Rational -> NoteTime
noteDuration t n = 
   (60.0 * (Ratio.toFloat t.unitNoteLength)) / 
    ((Ratio.toFloat t.tempoNoteLength) * (Basics.toFloat t.bpm)) * 
     (Ratio.toFloat n)


-- implementation

{- works from C major up to B major but not beyond 
   (F# major requires F->E#, C# major also requires C->B#)
  "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" 
-}
sharpScale : ChromaticScale
sharpScale = [ (C, Nothing),
               (C, Just Sharp),
               (D, Nothing),
               (D, Just Sharp), 
               (E, Nothing),  
               (F, Nothing), 
               (F, Just Sharp),  
               (G, Nothing),
               (G, Just Sharp), 
               (A, Nothing),
               (A, Just Sharp), 
               (B, Nothing)
             ]

-- "B#", "C#", "D", "D#", "E", "E#", "F#", "G", "G#", "A", "A#", "B" 
extremeSharpScale : ChromaticScale
extremeSharpScale =
   let 
     f pc = case pc of
       (F, Nothing) -> (E, Just Sharp)
       (C, Nothing) -> (B, Just Sharp)
       _ -> pc
   in
     List.map f sharpScale


{- works from C major down to Db major but not beyond 
   (Gb major requires B->Cb, Cb major also requires E->Fb)
   "C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B" 
-}
flatScale : ChromaticScale
flatScale = [ (C, Nothing),
              (D, Just Flat),
              (D, Nothing),
              (E, Just Flat), 
              (E, Nothing),  
              (F, Nothing), 
              (G, Just Flat),  
              (G, Nothing),
              (A, Just Flat), 
              (A, Nothing),
              (B, Just Flat), 
              (B, Nothing)
            ]

-- "C", "Db", "D", "Eb", "Fb", "F", "Gb", "G", "Ab", "A", "Bb", "Cb" 
extremeFlatScale : ChromaticScale
extremeFlatScale = 
   let 
     f pc = case pc of
       (E, Nothing) -> (F, Just Flat)
       (B, Nothing) -> (C, Just Flat)
       _ -> pc
   in
     List.map f flatScale


{- enharmonic equivalence - don't use bizarre sharp keys when we have reasonable flat ones -}
equivalentEnharmonic : KeyClass -> KeyClass
equivalentEnharmonic k = 
  case k of 
   (A, Just Sharp) -> (B, Just Flat)
   (C, Just Sharp) -> (D, Just Flat)
   (D, Just Sharp) -> (E, Just Flat)
   (G, Just Sharp) -> (A, Just Flat)
   _ -> k

majorIntervals : Intervals
majorIntervals = [2,2,1,2,2,2,1]

{- lookup for providing offsets from C in a chromatic scale 
   we have to translate a KeyClass to a string because otherwise
   it can't be used as a Dict key.  This is a problem in Elm -
   user-defined types should be attributable to a 'pseudo'
   type class of comparable -}
chromaticScaleDict : Dict String Int
chromaticScaleDict =
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
 
-- rotate the chromatic scale, starting from the supplied target character
rotateFrom : KeyClass -> ChromaticScale -> ChromaticScale
rotateFrom target scale =
  let
    index = elemIndex target scale
             |> withDefault 0
    listPair = splitAt index scale
  in
    List.append (snd listPair) (fst listPair)   

-- rotate an interval pattern to the left by the given amount
rotateLeftBy : Int -> Intervals -> Intervals
rotateLeftBy index ls =
  let
    listPair = splitAt index ls
  in
    List.append (snd listPair) (fst listPair)
    
{- convert e.g. [2,2,1,2,2,2,1] into [0,2,4,5,7,9,11]  
   so instead of tone/semitone intervals we have offsets into a chromatic scale
-}
partialSum : List Int -> List Int
partialSum l =
  List.map List.sum (tails (List.reverse l))
    |> List.reverse
    |> List.take (List.length l)  

-- find the pitch class at a given position in the scale   
lookUp : ChromaticScale -> Int -> KeyClass
lookUp s i =
  getAt s i
    |> withDefault (C, Nothing)
    
-- provide the Major scale for the pitch class  
majorScale : KeyClass -> Scale
majorScale target =
  let
    chromaticScale =
      if (target == (G, Just Flat) || target == (C, Just Flat)) then
        extremeFlatScale
      else if (isFlatMajorKey target) then
        flatScale
      else if (target ==  (F, Just Sharp) || target ==  (C, Just Sharp)) then
        extremeSharpScale
      else
        sharpScale
    f = lookUp (rotateFrom target chromaticScale)
  in
    List.map f (partialSum majorIntervals)

-- provide a Modal scale for the pitch class
modalScale : KeyClass -> Mode -> Scale
modalScale target mode =
  let
    distance = case mode of  -- the distance to move right round the major scale
      Minor -> 3
      Dorian -> 10
      Phrygian -> 8
      Lydian -> 7
      Mixolydian -> 5
      Aeolian -> 3
      Locrian -> 1
      _ -> 0  
    index = elemIndex target sharpScale
             |> withDefault 0
    majorKeyIndex = (index + distance) % 12
    majorKey = lookUp sharpScale majorKeyIndex
  in 
    majorScale (equivalentEnharmonic majorKey)  

{- return true if the key contains an accidental -}
accidentalKey : KeyClass -> Bool
accidentalKey k =
  let 
    (pc, acc) = k
  in
    case acc of
      Nothing -> False
      _ -> True

{- return true if the key represents a flat major key -} 
isFlatMajorKey : KeyClass -> Bool
isFlatMajorKey target = 
  let
     (pc, accidental) = target
  in
    case accidental of
      Nothing -> (pc == F)
      Just a -> (a == Flat)

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
    withDefault 0 (Dict.get pattern chromaticScaleDict)




      
      
 
