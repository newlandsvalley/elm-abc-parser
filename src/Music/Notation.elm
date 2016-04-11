module Music.Notation
  ( KeySet
  , KeyClass
  , MidiPitch
  , AbcTempo
  , NoteTime
  , notesInChromaticScale
  , keySet
  , modifiedKeySet
  , getKeySet
  , getKeySig
  , scale
  , isCOrSharpKey
  , accidentalImplicitInKey
  , accidentalInKeySet
  , naturaliseIfInKeySet
  , sharpenFlatEnharmonic
  , dotFactor
  , toMidiPitch
  , noteDuration
  , chordalNoteDuration
  , transposeKeySignatureBy
  ) where

{-|  Helper functions for making more musical sense of the parse tree

# Definition

# Data Types
@docs KeySet, KeyClass, MidiPitch, AbcTempo, NoteTime

# Functions
@docs notesInChromaticScale
    , keySet
    , modifiedKeySet
    , getKeySet
    , getKeySig
    , scale   
    , isCOrSharpKey
    , accidentalImplicitInKey
    , accidentalInKeySet
    , naturaliseIfInKeySet
    , sharpenFlatEnharmonic
    , dotFactor
    , toMidiPitch
    , noteDuration  
    , chordalNoteDuration
    , transposeKeySignatureBy

-}

import List.Extra exposing (getAt, splitAt, elemIndex, tails)
import List exposing (member, isEmpty)
import Maybe exposing (withDefault, oneOf)
import Maybe.Extra exposing (join, isJust)
import String exposing (contains, endsWith, fromChar)
import Dict exposing (Dict, fromList, get)
import Abc.ParseTree exposing (..)
import Ratio exposing (Rational, over, fromInt, toFloat, add)


import Debug exposing (..)

{-| a complete pitch class (the white note and the accidental) -}
type alias KeyClass = (PitchClass, Maybe Accidental)

type alias ChromaticScale = List KeyClass

type alias Scale = List KeyClass

{- the set of accidentals in a key signature 
   (or the set of accidentals (if any) previously set explicitly in the bar)
-}
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
notesInChromaticScale : Int
notesInChromaticScale = 12
    
{-| return the set of keys (pitch classes with accidental) that comprise the key signature -}
keySet : KeySignature -> KeySet
keySet ks =  
  scale ks
     |> List.filter accidentalKey

{-| return the set of keys (pitch classes with accidental) that comprise a modified key signature -}
modifiedKeySet : ModifiedKeySignature -> KeySet
modifiedKeySet ksm =  
  let
    (ksig, mods) = ksm
    ks = keySet ksig
  in
    if (isEmpty mods) then
      ks
    else
      List.foldr modifyKeySet ks mods

{-| get set of key accidentals from the (possibly modified) key (if there is one in the tune) -}
getKeySet : AbcTune -> KeySet
getKeySet t =
  let
    mksig = getKeySig t
  in case mksig of
    Just ksig -> modifiedKeySet ksig
    Nothing -> []

{-| get the key signature (if any) from the tune -}
getKeySig : AbcTune -> Maybe ModifiedKeySignature
getKeySig t =
  let
    headers = fst t
    f h = case h of
      Key mks -> Just mks
      _ -> Nothing
    ksigs = List.map f headers
      |> List.filter isJust
  in 
    List.head ksigs
     |> join


{-| return the set of keys (pitch classes) that comprise a complete 11-note scale -}
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


{-| return True if the key signature is a sharp key or a simple C Major key -}
isCOrSharpKey : KeySignature -> Bool
isCOrSharpKey ksig =
  let
    kset = keySet ksig
    -- if we make the default (for an empty list) as a samplemsharp then we return true for the scale of C Major
    (samplePC, sampleMacc) = List.head kset
                               |> withDefault (C, Just Sharp)
  in
    sampleMacc == Just Sharp

{-| return an accidental if it is implicitly there in the (modified) key signature 
    attached to the pitch class of the note -}
accidentalImplicitInKey : AbcNote -> ModifiedKeySignature -> Maybe Accidental
accidentalImplicitInKey n mks  =
    accidentalInKeySet n (modifiedKeySet mks)

{-| return an accidental if it is contained in the key set for the pitch class in question
    This is used as an implementation for the accidental checking in key signatures and 
    also directly for accidental checking for those accidentals explicitly marked earlier
    in the bar.
-}
accidentalInKeySet : AbcNote -> KeySet -> Maybe Accidental
accidentalInKeySet n ks =
  let
    f (pc, macc) = (toString pc, macc) 
    -- make the key comparable
    comparableks = List.map f ks
    -- make a dictionary now we have a comparable key
    lookup = Dict.fromList comparableks
  in
    -- just look up the (comparable) target
    Dict.get (toString n.pitchClass) lookup
      |> join

{- If the actual (PitchClass, maybe Accidental) within the note exists in the key set, 
   return the note with the pitch naturalised, otherwise leave it intact.
   This is the reverse operation to accidentalInKeySet and is used when looking up a note
   which is explictly marked with an accidental in order to display it as text
   because the accidental is implied by the key signature
-}
naturaliseIfInKeySet : AbcNote -> KeySet -> AbcNote
naturaliseIfInKeySet n ks =
  let 
    -- target = log "naturalise looking for " (n.pitchClass, n.accidental)
    target = (n.pitchClass, n.accidental)
  in
    if (List.member target ks) then
      {n | accidental = Nothing }
    else
      n

{- enharmonic equivalence for flattened notes
   We'll (for the moment) use the convention that most flat accidentals
   are presented in sharpened form
 -}
sharpenFlatEnharmonic : AbcNote -> AbcNote
sharpenFlatEnharmonic n = 
  let 
    target = (n.pitchClass, n.accidental)
  in
    case target of 
      (G, Just Flat) -> { n | pitchClass = F, accidental = Just Sharp }
      (D, Just Flat) -> { n | pitchClass = C, accidental = Just Sharp } 
      _ -> n

{- modify a key set with a new accidental -}
modifyKeySet : KeyAccidental -> KeySet -> KeySet
modifyKeySet target ks =
  let    
    -- filter out the given pitch class of the incoming key accidental
    f key = (fst key /= target.pitchClass)
    newks = List.filter f ks
  in
    -- if it's a natural, just remove any old sharp or flat key from the incoming key accidental
    if (target.accidental == Natural) then
      ks
    else
    -- otherwise, add the incoming key accidental
      (target.pitchClass, Just target.accidental) :: ks 


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

{-| convert an ABC note pitch to a MIDI pitch 
   AbcNote - the note in question
   ModifiedKeySignature - the key signature (possibly modified by extra accidentals) 
   KeySet - any notes in this bar which have previously been set explicitly to an accidental which are thus inherited by this note
   MidiPitch - the resulting pitch of the MIDI note
-}
toMidiPitch : AbcNote -> ModifiedKeySignature -> KeySet -> MidiPitch
toMidiPitch n mks barAccidentals =
  (n.octave * notesInChromaticScale) + midiPitchOffset n mks barAccidentals

{-| find a real world note duration by translating an ABC note duration using a tempo and unit note length  -}
noteDuration : AbcTempo -> Rational -> NoteTime
noteDuration t n = 
   (60.0 * (Ratio.toFloat t.unitNoteLength) *  (Ratio.toFloat n) ) / 
    ((Ratio.toFloat t.tempoNoteLength) * (Basics.toFloat t.bpm)) 
    


{-| find a real world duration of a note in a chord by translating an ABC note duration together with a chord duration using a tempo and unit note length -}
chordalNoteDuration : AbcTempo -> Rational -> Rational -> NoteTime
chordalNoteDuration t note chord = 
  (60.0 * (Ratio.toFloat t.unitNoteLength)* (Ratio.toFloat note) * (Ratio.toFloat chord)) / 
    ((Ratio.toFloat t.tempoNoteLength) * (Basics.toFloat t.bpm))

{-| transpose a key signature by a given distance -}
transposeKeySignatureBy : Int -> ModifiedKeySignature -> ModifiedKeySignature
transposeKeySignatureBy i mks =
  let
    (ks, keyaccs) = mks
    -- turn the key sig to a string pattern and look up its index
    pattern = (toString ks.pitchClass) ++ (accidentalPattern ks.accidental)
    index =  withDefault 0 (Dict.get pattern chromaticScaleDict)
    newIndex = (notesInChromaticScale + index + i) % notesInChromaticScale
    -- keep hold of the sharp / flat nature of the original scale
    scale =
      if (isCOrSharpKey ks) then
        sharpScale
      else
        flatScale
    -- now look up the transposed key at the new index
    (pc, ma) = 
      getAt scale newIndex
        |> withDefault (C, Nothing)
    -- modify the key accidentals likewise
    accs = List.map (transposeKeyAccidentalBy i) keyaccs
  in
    ( { pitchClass = pc, accidental = ma, mode = ks.mode }, accs )

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

{- find the pitch class at a given position in the scale   
   modulate the index to be in the range 0 <= index < notesInChromaticScale
   where a negative value rotates left from the maximum value in the scale 
-}
lookUp : ChromaticScale -> Int -> KeyClass
lookUp s i =
  let
    modi = i % notesInChromaticScale
    index =
      if (modi < 0) then
        notesInChromaticScale - modi
      else
        modi
  in
    getAt s index
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
    majorKeyIndex = (index + distance) % notesInChromaticScale
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
midiPitchOffset : AbcNote -> ModifiedKeySignature -> KeySet -> Int
midiPitchOffset n mks barAccidentals =
  let 
    inBarAccidental = accidentalInKeySet n barAccidentals
    inKeyAccidental = accidentalImplicitInKey n mks
    -- look first for an explicit note accidental, then for an explicit for the same note that occurred earlier in the bar and 
    -- finally look for an implicit accidental attached to this key signature
    maybeAccidental = oneOf [n.accidental, inBarAccidental, inKeyAccidental]
    accidental = accidentalPattern maybeAccidental
    pattern = (toString n.pitchClass) ++ accidental
  in
    withDefault 0 (Dict.get pattern chromaticScaleDict)

{- turn an optional accidental into a string pattern for use in lookups -}
accidentalPattern : Maybe Accidental -> String
accidentalPattern ma = 
  let
    f a = case a of
      Sharp -> "#"
      Flat -> "b"
      _ -> ""
  in
    withDefault "" (Maybe.map f ma)



{- transpose a key accidental  (a key signature modifier)
   not finished
-}
transposeKeyAccidentalBy : Int -> KeyAccidental -> KeyAccidental
transposeKeyAccidentalBy i ka =
  let
    pattern = (toString ka.pitchClass) 
    index = withDefault 0 (Dict.get pattern chromaticScaleDict)
    (modifier, scale) = 
      case ka.accidental of
       Sharp -> (1, sharpScale)
       Flat  -> (-1, flatScale)
       DoubleSharp -> (2, sharpScale)
       DoubleFlat -> (-2, flatScale)
       Natural -> (0, sharpScale)
    (pc, ma) = 
      getAt scale (index + modifier + i)
        |> withDefault (C, Nothing)
  in
    case ma of
      Nothing -> { pitchClass = pc, accidental = Natural }
      Just a -> { pitchClass = pc, accidental = a }



      
      
 
