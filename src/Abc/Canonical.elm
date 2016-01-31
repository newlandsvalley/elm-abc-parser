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

rational : Rational -> String
rational r =
  toString (numerator r) ++ "/" ++ toString (denominator r)

ratlist : List Rational -> String
ratlist rs = 
  let 
    f r acc = (rational r) ++ " " ++ acc
  in
    List.foldr f "" rs
    
meter : MeterSignature -> String
meter = rational

duration : NoteDuration -> String
duration nd = 
  if (denominator nd == 1) && (numerator nd == 1)
     then ""
  else if (denominator nd == 2) && (numerator nd == 1)
     then "/"
  else if (denominator nd == 1)
     then toString (numerator nd)
     else rational nd

key : KeySignature -> String
key k = 
   let
     acc = Maybe.map headerAccidental k.accidental
           |> withDefault ""
     md = Maybe.map mode k.mode 
           |> withDefault ""
   in
     k.keyClass ++ acc  ++ md

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
     tie = case a.tied of
       True -> "-"
       _ -> ""
  in
     acc
     ++ String.fromChar a.pitchClass
     ++ octave a.octave
     ++ duration a.duration
     ++ tie
     
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

decorate : String -> String
decorate s =
  if (String.length s == 1)
     then s
     else "!" ++ s ++ "!"
    
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
   Decoration s -> decorate s
   GraceNote isAcciaccatura m -> "{" ++ music m ++ "}"
   Slur c -> String.fromChar c
   ChordSymbol s -> enquote s
   Chord ns -> "[" ++ notes ns ++ "]"
   Inline h -> "[" ++ header h ++ "]"
   NoteSequence ms -> musics ms
   Spacer i -> " "
   -- _ -> ""

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
   Title s -> "T: " ++ s
   Tempo t -> "Q: " ++ (tempo t) 
   UserDefined s -> "U: " ++ s
   Voice s -> "V: " ++ s
   WordsAfter s -> "W: " ++ s
   WordsAligned s -> "w: " ++ s
   ReferenceNumber i -> "X: " ++ (toString i)
   Transcription s -> "Z: " ++ s
   Comment s -> "%" ++ s
   _ -> ""

tuneHeaders : List Header -> String
tuneHeaders  hs = 
  let 
    f h acc = (header h) ++ "\r\n" ++ acc
  in
    List.foldr f "" hs
    
bodyPart : BodyPart -> String
bodyPart bp = case bp of
  Score ml -> musics ml
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



