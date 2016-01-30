module Abc.Canonical 
   ( fromTune
   , fromResult
   ) where
   
{-|  Library for converting an ABC Tune parse tree to a canonical ABC string,
   
   # Definition
   
   # Functions
   @docs fromTune, fromResult
   
-}

import Abc.ParseTree exposing (..)
import Ratio exposing (Rational, numerator, denominator)
import Maybe exposing (withDefault)
import String exposing (fromChar, fromList, repeat)

enquote : String -> String
enquote s = "\"" ++ s ++ "\""

mode : Mode -> String
mode m = case m of
  Major -> "major"
  Minor -> "minor"
  Ionian -> "ionian"
  Dorian -> "dorian"
  Phrygian -> "phrygian"
  Lydian -> "lydian"
  Mixolydian -> "mixolydian"
  Aeolian -> "aeolian"
  Locrian -> "locrian"
  
bar : Bar -> String
bar b = 
  let 
    it = withDefault "" (Maybe.map toString b.iteration)
  in
    b.separator ++ it
    
accidental : Accidental -> String
accidental a = case a of
  Sharp -> "^"
  Flat -> "b"
  DoubleSharp -> "^^"
  DoubleFlat -> "bb"
  Natural -> "="
  
headerAccidental : Accidental -> String
headerAccidental a = case a of
  Sharp -> "#"
  Flat -> "b"
  _ -> ""
  
tuplet : TupletSignature -> String
tuplet t =
  "(" 
    ++ (toString t.p)
    ++ ":"  
    ++ (toString t.q)
    ++ ":" 
    ++ (toString t.r)
    
tempo : TempoSignature -> String
tempo t =
  let
    text = withDefault "" (Maybe.map (\s -> " " ++ s) t.marking)
  in 
    ratlist t.noteLengths
        ++ "="
        ++ toString t.bpm
        ++ text

ratlist : List Rational -> String
ratlist rs = 
  let 
    f r acc = (toString r) ++ " " ++ acc
  in
    List.foldr f "" rs
    
meter : MeterSignature -> String
meter = toString

duration : NoteDuration -> String
duration nd = 
  if (denominator nd == 1)
     then toString (numerator nd)
     else toString nd

key : KeySignature -> String
key k = "todo"  -- make KeySignature into a record

octave : Int -> String
octave i =
   if ((i == 4) || (i == 5))
      then ""
   else if i > 5 then 
      repeat (i - 5) "'"
   else
      repeat (4 - i) ","      
   

abcNote : AbcNote -> String
abcNote a =
  let
     acc = withDefault ""
            (Maybe.map accidental a.accidental)
  in
     acc
     ++ toString a.pitchClass
     ++ octave a.octave
     ++ duration a.duration
     
notes : List AbcNote -> String
notes ns = 
  let 
    f a acc = (abcNote a) ++ acc
  in
    List.foldr f "" ns   
     
rest : Int -> String
rest n =
  let
     num =
       if (n > 1)
          then toString n
          else ""
  in
    "z" ++ num
    
musics : List Music-> String
musics ms = 
  let 
    f m acc = (music m) ++ acc
  in
    List.foldr f "" ms      
     
music : Music -> String
music m = case m of
   Barline b -> bar b
   Note a -> abcNote a
   BrokenRhythmPair a1 c a2 -> abcNote a1 ++ fromChar c ++ abcNote a2
   Rest r -> rest r
   Tuplet tup ns -> tuplet tup ++ notes ns
   Decoration s -> enquote s
   GraceNote isAcciaccatura m -> "{" ++ music m ++ "}"
   ChordSymbol s -> enquote s
   Inline h -> "[" ++ header h ++ "]"
   NoteSequence ms -> musics ms
   Spacer i -> " "
   _ -> ""

header : Header -> String
header h = case h of
   Area s -> "A: " ++ s
   Book s -> "B: " ++ s
   Composer s -> "C: " ++ s
   Discography s -> "D: " ++ s
   FileUrl s -> "F: " ++ s
   Group s -> "G: " ++ s
   History s -> "H: " ++ s
   Instruction s -> "I: " ++ s
   Key k -> "K: " ++ (key k)
   UnitNoteLength d -> "L: " ++ (duration d)
   Meter m -> "M: " ++ (meter m)   
   Macro s -> "m: " ++ s
   Notes s -> "N: " ++ s
   Origin s -> "O: " ++ s
   Parts s -> "P: " ++ s
   Rhythm s -> "R: " ++ s
   Remark s -> "r: " ++ s
   Source s -> "S: " ++ s
   Tempo t -> "T: " ++ (tempo t) 
   UserDefined s -> "U: " ++ s
   Voice s -> "V: " ++ s
   WordsAfter s -> "W: " ++ s
   WordsAligned s -> "w: " ++ s
   ReferenceNumber i -> "X: " ++ (toString i)
   Transcription s -> "Z: " ++ s
   _ -> ""

tuneHeaders : List Header -> String
tuneHeaders  hs = 
  let 
    f h acc = (header h) ++ "\r\n" ++ acc
  in
    List.foldr f "" hs
    
bodyPart : BodyPart -> String
bodyPart bp = case bp of
  Score ml -> "todo"
  BodyInfo h ->  header h
  
tuneBody : TuneBody -> String
tuneBody b = 
  let 
    f bp acc = (bodyPart bp) ++ "\r\n" ++ acc
  in
    List.foldr f "" b
  
-- Exported Functions

{-| translate an ABC Tune parse tree to a canonical ABC String -}
fromTune : AbcTune -> String
fromTune t = 
  tuneHeaders (fst t) ++ tuneBody (snd t)


{-| translate a parse Result containing an ABC Tune parse tree to a Result containing a canonical ABC String -}
fromResult : Result String AbcTune -> Result String String
fromResult r =
  Result.map fromTune r



