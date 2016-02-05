module Music.Notation
  ( KeyClass
  , keySet
  , scale
  ) where

{-|  Helper functions for making more musical sense of the parse tree

# Definition

# Data Types
@docs KeyClass

# Functions
@docs keySet, scale

-}

import List.Extra exposing (getAt, splitAt, elemIndex, tails)
import List exposing (member)
import Maybe exposing (withDefault)
import String exposing (contains, endsWith, fromChar)
import Abc.ParseTree exposing (Mode (..), Accidental (..), KeySignature, PitchClass (..))

{-| a complete pitch class (the white note and the accidental) -}
type alias KeyClass = (PitchClass, Maybe Accidental)

type alias ChromaticScale = List KeyClass

type alias Scale = List KeyClass

{- the set of accidentals in a key signature -}
type alias KeySet = List KeyClass

type alias Intervals = List Int


-- EXPORTED FUNCTIONS
    
{-| return the set of keys (pitches) that comprise the key signature -}
keySet : KeySignature -> KeySet
keySet ks =  
  scale ks
     |> List.filter accidentalKey

{-| return the set of pitches that comprise a complete 11-note scale -}
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
      Locrian -> 2
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


      
      
 
