module Abc.ParseTree
    exposing
        ( AbcTune
        , TuneHeaders
        , TuneBody
        , BodyPart(..)
        , MusicLine
        , Header(..)
        , Music(..)
        , AbcNote
        , AbcChord
        , Bar
        , Thickness(..)
        , Repeat(..)
        , NoteDuration
        , KeySignature
        , ModifiedKeySignature
        , KeyAccidental
        , KeySet
        , MeterSignature
        , TempoSignature
        , TupletSignature
        , AnnotationPlacement(..)
        , Mode(..)
        , Accidental(..)
        , PitchClass(..)
        , Broken(..)
        , middlecOctave
        )

{-| The ABC parse tree.

# Data Types
@docs AbcTune
    , TuneHeaders
    , TuneBody
    , BodyPart
    , MusicLine
    , Header
    , Music
    , AbcNote
    , AbcChord
    , Bar
    , Thickness
    , Repeat
    , NoteDuration
    , KeySignature
    , ModifiedKeySignature
    , KeyAccidental
    , KeySet
    , MeterSignature
    , TempoSignature
    , TupletSignature
    , AnnotationPlacement
    , Mode
    , Accidental
    , PitchClass
    , Broken

# Functions  (constants)
@docs middlecOctave

-}

import Ratio exposing (Rational)


{-| A Tune.
-}
type alias AbcTune =
    ( TuneHeaders, TuneBody )


{-| A List of Tune Headers.
-}
type alias TuneHeaders =
    List Header


{-| A Tune Body.
-}
type alias TuneBody =
    List BodyPart


{-| A Tune Body part
-}
type BodyPart
    = Score MusicLine
    | BodyInfo Header


{-| A line of musical score up to eol.
-}
type alias MusicLine =
    List Music


{-| A Note.
-}
type alias AbcNote =
    { pitchClass : PitchClass
    , accidental : Maybe Accidental
    , octave : Int
    , duration : NoteDuration
    , tied :
        Bool
        -- to the next note
    }


{-| A Chord.
-}
type alias AbcChord =
    { notes : List AbcNote
    , duration : NoteDuration
    }


{-| An Annotation placement.
-}
type AnnotationPlacement
    = AboveNextSymbol
    | BelowNextSymbol
    | LeftOfNextSymbol
    | RightOfNextSymbol
    | Discretional


{-| The 'score' part of Music.
-}
type Music
    = Barline Bar
    | Note AbcNote
    | BrokenRhythmPair AbcNote Broken AbcNote
    | Rest NoteDuration
    | Tuplet TupletSignature (List AbcNote)
    | Decoration String
    | Slur Char
    | GraceNote Bool (List AbcNote)
      -- Music restricted to note sequences or chords
    | Annotation AnnotationPlacement String
    | ChordSymbol String
    | Chord AbcChord
    | Inline Header
    | Spacer Int
    | Ignore
    | Continuation


{-| A bar line Thickness.
-}
type Thickness
    = Thin
    | ThinThin
    | ThinThick
    | ThickThin


{-| A Repeat in a Bar line.
-}
type Repeat
    = Begin
    | End
    | BeginAndEnd


{-| A Bar line:

*  thickness - the thickness of vertical lines in the bar
*  repeat - the type (if any) of a repeat marker for the section
*  iteration - the section end may be iteration 1 or 2.
-}
type alias Bar =
    { thickness : Thickness
    , repeat : Maybe Repeat
    , iteration : Maybe Int
    }


{-| A Mode.
-}
type Mode
    = Major
    | Minor
    | Ionian
    | Dorian
    | Phrygian
    | Lydian
    | Mixolydian
    | Aeolian
    | Locrian


{-| An Accidental.
-}
type Accidental
    = Sharp
    | Flat
    | DoubleSharp
    | DoubleFlat
    | Natural


{-| A white note on the piano.
-}
type PitchClass
    = A
    | B
    | C
    | D
    | E
    | F
    | G


{-| A Key Signature.
-}
type alias KeySignature =
    { pitchClass : PitchClass
    , accidental : Maybe Accidental
    , mode : Mode
    }


{-| A Key Signature with modifications (possibly empty)
    This is used for non-diatonic modes where intervals may be greater than two semitones
    (for example as found in Klezmer).
-}
type alias ModifiedKeySignature =
    ( KeySignature, List KeyAccidental )


{-| A Key Accidental (A modification to a standard key for one pitch in the scale).
-}
type alias KeyAccidental =
    ( PitchClass, Accidental )


{-| A set of accidentals within a key signature.
-}
type alias KeySet =
    List KeyAccidental


{-| A Meter Signature - e.g. 3/4.
-}
type alias MeterSignature =
    ( Int, Int )


{-| A Tempo Signature - for example:

*  1/4=120
*  1/4 3/8 1/4 3/8=40   (up to 4 note lengths allowed)
*  "Allegro" 1/4=120
*  3/8=50 "Slowly".
-}
type alias TempoSignature =
    { noteLengths : List Rational
    , bpm : Int
    , marking : Maybe String
    }


{-| A Note Duration - e.g. 1/4.
-}
type alias NoteDuration =
    Rational


{-| A tuplet signature:
    put p notes into the time of q the next r notes.
-}
type alias TupletSignature =
    ( Int, Int, Int )


{-| A broken rhythm operator - one or more < or >.
-}
type Broken
    = LeftArrow Int
    | RightArrow Int


{-| An ABC Tune Header.
-}
type Header
    = Area String
    | Book String
    | Composer String
    | Discography String
    | FileUrl String
    | Group String
    | History String
    | Instruction String
      -- Directive
    | Key ModifiedKeySignature
      -- a standard key signature possibly modified with accidentals
    | UnitNoteLength NoteDuration
    | Meter (Maybe MeterSignature)
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
    | Voice String
      -- voice properties
    | WordsAfter String
      -- words after notes
    | WordsAligned String
      -- words aligned with notes
    | ReferenceNumber Int
    | Transcription String
    | FieldContinuation String
    | Comment String
    | UnsupportedHeader


{-| The octave number of middle C in MIDI parlance.
-}
middlecOctave : Int
middlecOctave =
    5
