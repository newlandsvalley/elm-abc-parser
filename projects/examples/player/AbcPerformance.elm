module AbcPerformance
    exposing
        ( fromAbc
        , fromAbcResult
        , melodyFromAbc
        , melodyFromAbcResult
        )

{-| conversion of a ABC Tune parse tree to a performance

# Definition

# Functions
@docs fromAbc
    , fromAbcResult
    , melodyFromAbc
    , melodyFromAbcResult

-}

{- note on implementation

   I originally folded from the right, which is conceptually much simpler and more efficient.
   However, I found that, because of the fact that an explicitly marked accidental influences
   notes of the same pitch class later on in the same bar, I was forced to fold from the left.
   This is more inefficient, as I have to reverse everything at the end.
-}

import Abc.ParseTree exposing (..)
import Abc exposing (ParseError)
import Music.Notation exposing (..)
import Music.Accidentals exposing (..)
import Melody exposing (..)
import Repeats exposing (..)
import RepeatTypes exposing (..)
import String exposing (fromChar, toUpper)
import Ratio exposing (Rational, over, fromInt, toFloat, add)
import Maybe exposing (withDefault)
import Tuple exposing (first, second)


type alias TranslationState =
    { modifiedKeySignature : ModifiedKeySignature
    , tempo : AbcTempo
    , tempoModifier : Float
    , nextBarNumber : Int
    , thisBar : ABar
    , repeatState : RepeatState
    }



-- default to 1/4=120


defaultTempo : AbcTempo
defaultTempo =
    { tempoNoteLength = over 1 4
    , bpm = 120
    , unitNoteLength = over 1 8
    }



-- default to C Major (i.e. no accidental modifiers)


defaultKey : ModifiedKeySignature
defaultKey =
    ( { pitchClass = C
      , accidental = Nothing
      , mode = Major
      }
    , []
    )


defaultBar : Int -> ABar
defaultBar i =
    { number = i
    , repeat = Nothing
    , iteration = Nothing
    , accidentals = Music.Accidentals.empty
    , notes = []
    }


isEmptyBar : ABar -> Bool
isEmptyBar b =
    List.length b.notes == 0



{- update the state of the player when we come across a header (either at the start or inline)
   which affects the tune tempo or the pitch of a note (i.e. they key)
-}


updateState : Header -> ( MelodyLine, TranslationState ) -> ( MelodyLine, TranslationState )
updateState h acc =
    let
        ( melody, state ) =
            acc

        tempo =
            state.tempo
    in
        case h of
            UnitNoteLength d ->
                ( melody, { state | tempo = { tempo | unitNoteLength = d } } )

            Tempo t ->
                let
                    tnl =
                        List.foldl Ratio.add (fromInt 0) t.noteLengths
                in
                    ( melody, { state | tempo = { tempo | tempoNoteLength = tnl, bpm = t.bpm } } )

            -- ignore accidental note modifiers in key signatures for the moment - they're little used
            Key mk ->
                ( melody, { state | modifiedKeySignature = mk } )

            _ ->
                acc



{- we need to take note of any accidentals so far in the bar because these may influence
   later notes in that bar.  Build the KeyAccidental for the accidental of the pitch class in question
   and add it to the list
-}


addNoteToBarAccidentals : SingleNote -> Accidentals -> Accidentals
addNoteToBarAccidentals n accs =
    case ( n.pc, n.accidental ) of
        ( Just pitchClass, Just acc ) ->
            Music.Accidentals.add pitchClass acc accs

        _ ->
            accs



{- ditto for note events (single notes or chords) -}


addNoteEventToBarAccidentals : NoteEvent -> Accidentals -> Accidentals
addNoteEventToBarAccidentals ne accs =
    case ne of
        ANote note _ ->
            addNoteToBarAccidentals note accs

        AChord ns ->
            List.foldl (addNoteToBarAccidentals) accs ns



{- ditto for lists of note events -}


addNoteEventsToBarAccidentals : List NoteEvent -> Accidentals -> Accidentals
addNoteEventsToBarAccidentals nes accs =
    List.foldl (addNoteEventToBarAccidentals) accs nes



{- add a note event to the state - add the note to the growing list of notes in the current bar
   and if the note has an explicit accidental marker, add it to the list of accidentals
-}


addNoteToState : NoteEvent -> TranslationState -> TranslationState
addNoteToState n state =
    let
        line =
            state.thisBar.notes

        thisBar =
            state.thisBar

        accidentals =
            addNoteEventToBarAccidentals n thisBar.accidentals
    in
        { state | thisBar = { thisBar | notes = n :: line, accidentals = accidentals } }



{- ditto for a list of notes -}


addNotesToState : List NoteEvent -> TranslationState -> TranslationState
addNotesToState ns state =
    let
        line =
            state.thisBar.notes

        thisBar =
            state.thisBar

        accidentals =
            addNoteEventsToBarAccidentals ns thisBar.accidentals
    in
        { state | thisBar = { thisBar | notes = List.append ns line, accidentals = accidentals } }



{- build a new bar from the bar number and the next ABC bar that we recognise.
   If the last bar was empty, retain its repeat markings, because otherwise we drop this bar
-}


buildNewBar : Int -> Bar -> ABar -> ABar
buildNewBar nextBarNumber abcBar lastBar =
    let
        nextBar =
            defaultBar nextBarNumber
    in
        if (isEmptyBar lastBar) then
            case ( lastBar.repeat, abcBar.repeat ) of
                ( Just End, Just Begin ) ->
                    { nextBar | repeat = Just BeginAndEnd, iteration = abcBar.iteration }

                ( Just x, _ ) ->
                    { nextBar | repeat = Just x, iteration = abcBar.iteration }

                _ ->
                    { nextBar | repeat = abcBar.repeat, iteration = abcBar.iteration }
        else
            { nextBar | repeat = abcBar.repeat, iteration = abcBar.iteration }



{- translate a chord (parallel sequence) -}


translateChord : TranslationState -> List AbcNote -> Maybe NoteDuration -> List NoteEvent
translateChord state notes maybeChordDur =
    let
        -- a chord can have a duration over and above that of any individual note in the chord
        chordDuration =
            case maybeChordDur of
                Nothing ->
                    fromInt 1

                Just chordDur ->
                    chordDur

        f abc =
            let
                duration =
                    (chordalNoteDuration state.tempo abc.duration chordDuration) * state.tempoModifier

                barAccidentals =
                    state.thisBar.accidentals
            in
                { time = duration, pitch = toMidiPitch abc state.modifiedKeySignature barAccidentals, pc = Just abc.pitchClass, accidental = abc.accidental }
    in
        [ AChord (List.map f notes) ]



{- translate a sequence of notes as found in tuplets (sequential) -}


translateNoteSequence : List AbcNote -> TranslationState -> TranslationState
translateNoteSequence notes state =
    List.foldl translateNote state notes



{- translate a single note and embed it into the state -}


translateNote : AbcNote -> TranslationState -> TranslationState
translateNote abc state =
    let
        duration =
            (noteDuration state.tempo abc.duration) * state.tempoModifier

        barAccidentals =
            state.thisBar.accidentals

        note =
            ANote { time = duration, pitch = toMidiPitch abc state.modifiedKeySignature barAccidentals, pc = Just abc.pitchClass, accidental = abc.accidental } abc.tied
    in
        addNoteToState note state



{- translate Music items from the parse tree to a melody line - a sequence
   of bars containing notes, rests and chords where notes are in a MIDI-friendly format
-}


translateMusic : Music -> ( MelodyLine, TranslationState ) -> ( MelodyLine, TranslationState )
translateMusic m acc =
    let
        ( melodyLine, state ) =
            acc
    in
        case m of
            Note abc ->
                let
                    newState =
                        translateNote abc state
                in
                    ( melodyLine, newState )

            Rest r ->
                let
                    duration =
                        (noteDuration state.tempo r) * state.tempoModifier

                    note =
                        ANote { time = duration, pitch = 0, pc = Nothing, accidental = Nothing } False

                    newState =
                        addNoteToState note state
                in
                    ( melodyLine, newState )

            Tuplet signature tnotes ->
                let
                    ( p, q, r ) =
                        signature

                    tupletStateStart =
                        { state | tempoModifier = (Basics.toFloat q / Basics.toFloat p) }

                    tupletStateEnd =
                        translateNoteSequence tnotes tupletStateStart

                    -- recover the original tempo in the state but we save any accidentals we've come across
                    newState =
                        { tupletStateEnd | tempoModifier = 1 }
                in
                    ( melodyLine, newState )

            BrokenRhythmPair n1 b n2 ->
                case b of
                    LeftArrow i ->
                        let
                            leftStateStart =
                                { state | tempoModifier = (1 - Ratio.toFloat (dotFactor i)) }

                            leftStateEnd =
                                translateNote n1 leftStateStart

                            rightStateStart =
                                { leftStateEnd | tempoModifier = (1 + Ratio.toFloat (dotFactor i)) }

                            rightStateEnd =
                                translateNote n2 rightStateStart

                            -- recover the original tempo in the state but save any accidentals we've come across
                            newState =
                                { rightStateEnd | tempoModifier = 1 }
                        in
                            ( melodyLine, newState )

                    RightArrow i ->
                        let
                            leftStateStart =
                                { state | tempoModifier = (1 + Ratio.toFloat (dotFactor i)) }

                            leftStateEnd =
                                translateNote n1 leftStateStart

                            rightStateStart =
                                { leftStateEnd | tempoModifier = (1 - Ratio.toFloat (dotFactor i)) }

                            rightStateEnd =
                                translateNote n2 rightStateStart

                            -- recover the original tempo in the state but save any accidentals we've come across
                            newState =
                                { rightStateEnd | tempoModifier = 1 }
                        in
                            ( melodyLine, newState )

            Chord abcChord ->
                let
                    chord =
                        translateChord state abcChord.notes (Just abcChord.duration)

                    newState =
                        addNotesToState chord state
                in
                    ( melodyLine, newState )

            Barline b ->
                let
                    -- don't add to the melody the existing bar accumulated by the state if it's empty
                    newMelody =
                        if (isEmptyBar state.thisBar) then
                            melodyLine
                        else
                            let
                                rb =
                                    state.thisBar
                            in
                                state.thisBar :: melodyLine

                    -- don't increment the bar number if it's an empty bar
                    nextBarNumber =
                        if (isEmptyBar state.thisBar) then
                            state.nextBarNumber
                        else
                            state.nextBarNumber + 1

                    {-
                       nextBar = defaultBar nextBarNumber
                       newBar = { nextBar | repeat = b.repeat, iteration = b.iteration }
                    -}
                    -- build a new Bar from the incoming AbcBar, retaining any unused state from the last bar if it was empty (and hence to be dropped)
                    newBar =
                        buildNewBar nextBarNumber b state.thisBar

                    -- index the last bar if it was not empty
                    repeatState =
                        if (isEmptyBar state.thisBar) then
                            state.repeatState
                        else
                            indexBar state.thisBar state.repeatState

                    newState =
                        { state | thisBar = newBar, nextBarNumber = nextBarNumber, repeatState = repeatState }
                in
                    ( newMelody, newState )

            Inline header ->
                updateState header acc

            _ ->
                acc



-- translate an entire melody line from the tune body (up to an end of line)


toMelodyLine : MusicLine -> ( MelodyLine, TranslationState ) -> ( MelodyLine, TranslationState )
toMelodyLine ml state =
    let
        ( melody, s ) =
            List.foldl translateMusic state ml
    in
        ( melody, s )


reverseMelody : MelodyLine -> MelodyLine
reverseMelody =
    let
        reverseBar b =
            { b | notes = List.reverse b.notes }
    in
        List.map reverseBar
            >> List.reverse



{- translate an AbcTune to a more playable melody line
   which is a list of notes (or rests) and their durations
-}


fromAbc : AbcTune -> ( MelodyLine, Repeats )
fromAbc tune =
    let
        -- set a default state for case where there are no tune headers
        defaultState =
            ( []
            , { modifiedKeySignature = defaultKey
              , tempo = defaultTempo
              , tempoModifier = 1.0
              , nextBarNumber = 0
              , thisBar = defaultBar 0
              , repeatState = defaultRepeatState
              }
            )

        -- update this from the header state if we have any headers
        headerState =
            List.foldl updateState defaultState (first tune)

        f bp acc =
            case bp of
                -- process a line from the melody using the current state
                Score musicLine ->
                    let
                        ( existingLine, state ) =
                            acc

                        ( newLine, newState ) =
                            toMelodyLine musicLine acc
                    in
                        ( newLine, newState )

                -- update the state if we have an inline header
                BodyInfo header ->
                    updateState header acc
    in
        let
            ( music, state ) =
                List.foldl f headerState (second tune)

            -- ensure we don't forget the residual closing bar (still kept in the state) which may yet contain music
            fullMusic =
                reverseMelody (state.thisBar :: music)

            -- finalise the repeat state with the last bar
            repeatState =
                finalise state.thisBar state.repeatState

            -- _ = log "repeats" (List.reverse repeatState.repeats)
        in
            ( fullMusic, (List.reverse repeatState.repeats) )


melodyFromAbc : Bool -> AbcTune -> ( MelodyLine, Repeats )
melodyFromAbc expandRepeats tune =
    let
        mr =
            fromAbc tune
    in
        if (expandRepeats) then
            ( buildRepeatedMelody mr, [] )
        else
            mr


fromAbcResult : Result ParseError AbcTune -> Result ParseError ( MelodyLine, Repeats )
fromAbcResult r =
    Result.map fromAbc r


melodyFromAbcResult : Result ParseError AbcTune -> Result ParseError MelodyLine
melodyFromAbcResult r =
    -- Result.map (fromAbc >> first) r
    Result.map (fromAbc >> buildRepeatedMelody) r
