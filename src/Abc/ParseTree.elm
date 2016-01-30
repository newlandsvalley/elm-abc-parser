module Abc.ParseTree
    ( AbcTune
    , TuneHeaders
    , TuneBody
    , BodyPart (..)
    , MusicLine
    , Header (..)
    , Music (..)
    , AbcNote
    , Bar
    , NoteDuration
    , KeySignature
    , MeterSignature
    , TempoSignature
    , TupletSignature
    , Mode (..)
    , Accidental (..)
    ) where

{-|  The ABC parser and ABC notation tree

# Definition

# Data Types
@docs AbcTune
    , TuneHeaders
    , TuneBody
    , BodyPart
    , MusicLine
    , Header
    , Music
    , AbcNote
    , Bar
    , NoteDuration
    , KeySignature
    , MeterSignature
    , TempoSignature
    , TupletSignature
    , Mode
    , Accidental

# Functions

-}


import Ratio exposing (Rational)

{-| AbcTune -}
type alias AbcTune = (TuneHeaders, TuneBody)

{-| a List of ABC Tune Header -}
type alias TuneHeaders = List Header

{-| an ABC Tune Body -}
type alias TuneBody = List BodyPart

{-| A Tune Body part -}
type BodyPart
  =  Score MusicLine
  |  BodyInfo Header

{-| a line of musical score up to eol -} 
type alias MusicLine = List Music

{-| a Note -}
type alias AbcNote =
  {  pitchClass : Char
  ,  accidental : Maybe Accidental
  ,  octave : Int
  ,  duration : NoteDuration
  ,  tied : Bool   -- to the next note
}

{-| the 'score' part of Music -}
type Music 
  = Barline Bar
  | Note AbcNote
  | BrokenRhythmPair AbcNote Char AbcNote
  | Rest Int
  | Tuplet TupletSignature (List AbcNote)
  | Decoration String
  | Slur Char
  | GraceNote Bool Music         -- Music restricted to note sequences or chords
  | ChordSymbol String
  | Chord (List AbcNote)
  | Inline Header
  | NoteSequence (List Music)
  | Spacer Int

{-| a Bar line -}
type alias Bar = 
  { separator : String
  , iteration : Maybe Int
  }


{-| a Mode -}
type Mode = 
    Major
  | Minor
  | Ionian
  | Dorian
  | Phrygian
  | Lydian
  | Mixolydian
  | Aeolian
  | Locrian


{-| An Accidental -}
type Accidental = 
    Sharp
  | Flat
  | DoubleSharp
  | DoubleFlat
  | Natural


{-| a Key Signature -}
type alias KeySignature = 
  { keyClass : String
  , accidental : Maybe Accidental
  , mode : Maybe Mode
  } 

{-| a Meter Signature - e.g. 3/4 -}
type alias MeterSignature = Rational

{-| a Tempo Signature - e.g. 1/4=120
    or 1/4 3/8 1/4 3/8=40   (up to 4 note lengths allowed)
    or "Allegro" 1/4=120
    or 3/8=50 "Slowly" -}
type alias TempoSignature = 
  { noteLengths: List Rational
  , bpm : Int
  , marking : Maybe String
  }

{-| a Note Duration - e.g. 1/4 -}
type alias NoteDuration = Rational

{-| a tuplet signature -}
type alias TupletSignature =
  { p : Int      -- put p notes
  , q : Int        -- into the time of q
  , r : Int            -- for the next r notes
  }



{-| an ABC Tune Header -}
type Header =
      Area String
    | Book String
    | Composer String
    | Discography String
    | FileUrl String
    | Group String
    | History String
    | Instruction String                 -- Directive
    | Key KeySignature          
    | UnitNoteLength NoteDuration                   
    | Meter MeterSignature                          
    | Macro String                       
    | Notes String                    
    | Origin String                  
    | Parts String
    | Tempo TempoSignature                
    | Rhythm String                    
    | Remark String                     
    | Source String                  
    | SymbolLine String
    | Title String                   
    | UserDefined String                
    | Voice String                      -- voice properties
    | WordsAfter String                 -- words after notes
    | WordsAligned String               -- words aligned with notes
    | ReferenceNumber Int
    | Transcription String
    | Comment String



