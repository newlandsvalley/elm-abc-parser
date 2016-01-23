module AbcTuneParser
    ( AbcTune
    , TuneHeaders
    , TuneBody
    , Header
    , MeterSignature
    , TempoSignature
    , NoteDuration
    , KeySignature
    , Accidental
    , Mode
    , parse
    ) where

{-|  Library for parsing ABC transcriptions using parser combinators
     see http://abcnotation.com/wiki/abc:standard:v2.1

# Definition

# Data Types
@docs AbcTune, TuneHeaders, TuneBody, MeterSignature, TempoSignature, NoteDuration, Header, KeySignature, Accidental, Mode

# Functions
@docs parse

-}

import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Infix exposing (..)
import Combine.Num exposing (..)
import String exposing (fromList, toList)
import Debug exposing (..)
import Maybe exposing (withDefault)
import Result exposing (Result)

{-| an ABC Tune Body -}
type alias TuneBody = List MusicLine

type alias MusicLine = List Music

type Music 
  = Barline Bar
  | Note String Int
  | Tuplet Int
  | Tie
  | Spacer Int
  | Stuff String


type Bar
    = SingleBar                     -- |
    | DoubleBar Bool Bool           -- thick? thick?  ||, |], [|
    | RepeatStart                   -- |:
    | RepeatEnd                     -- :|
    | StartAndEnd                   -- ::
    | Iteration Int                 -- |1  or |2

{-| a line of Music 
type Music
    = Chord String                   --  Chord Chord 
    | Barline String                 --  Barline Barline  
    | Tie Music
    | Slur Music
    | Beam Music
    | Grace Music
    | Tuplet String Music            --  Tuplet Duration Music  
    | Decorate String Music          --  Decorate Decoration Music
    | Annotate String Music          --  Annotate Annotation Music
    | ChordSymbol String Music       --  ChordSymbol ChordSymbol Music
    | Sequence (List Music)          --  beam? music
-}

type alias Fraction = (Int, Int)

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
  | Natural


{-| a Key Signature -}
type alias KeySignature = (String, Maybe Accidental, Maybe Mode) 

{-| a Meter Signature - e.g. 3/4 -}
type alias MeterSignature = Fraction

{-| a Tempo Signature - e.g. 1/4=120
    or 1/4 3/8 1/4 3/8=40   (up to 4 note lengths allowed)
    or "Allegro" 1/4=120
    or 3/8=50 "Slowly" -}
type alias TempoSignature = 
  { noteLengths: List Fraction
  , bpm : Int
  , marking : Maybe String
  }

{-| a Note Duration - e.g. 1/4 -}
type alias NoteDuration = Fraction

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
    | Notes String                      -- ^ Notes
    | Origin String                     -- ^ Origin of tune.
    | Parts String
    | Tempo TempoSignature                
    | Rhythm String                     -- ^ Rhythm type of tune.
    | Remark String                     
    | Source String                     -- ^ Source material.
    | SymbolLine String
    | Title String                      -- ^ Title of tune.
    | UserDefined String                
    | Voice String                      -- VoiceProperties
    | WordsAfter String                 -- words after notes
    | WordsAligned String               -- words aligned with notes
    | ReferenceNumber Int
    | Transcription String

{-| a List of ABC Tune Header -}
type alias TuneHeaders = List Header

{-| AbcTune -}
type alias AbcTune = (TuneHeaders, TuneBody)

-- BODY
body : Parser TuneBody    
body = many1 (musicLine <* endLine) <* rest

musicLine : Parser (List Music)
musicLine = many musicItem

musicItem : Parser Music
musicItem = 
    log "music item" <$>
    (
    choice 
       [ 
         barline
       , anote
       , tuplet
       , tie
       , bodyStuff
       , spacer
       ]       
    )

barline : Parser Music
barline = Barline <$> (choice [complexBar, simpleBar])

complexBar : Parser Bar
complexBar = buildBar <$> (log "complex bar" <$> (choice
               [
                 string "||"
               , string "[|"  
               , string "|]"  
               , string "|:" 
               , string ":|" 
               , string "::" 
               , char '|' *> regex "[0-9]"
               ]
              ))

simpleBar : Parser Bar
simpleBar = succeed SingleBar <* char '|'
             <?> "barline"

tie : Parser Music
tie = succeed Tie <* char '-'

-- HEADERS

-- Header attributes
fraction : Parser Fraction
fraction = buildFraction <$> int <*> char '/' <*> int <* whiteSpace

meterSignature : Parser MeterSignature
meterSignature = fraction

noteDuration : Parser NoteDuration
noteDuration = fraction

-- perhaps we need many1 rather than many here for conformance to the 2.1 spec
-- leaving out the note duration is common (and defaults to 1/4) so is more forgiving
tempoSignature : Parser TempoSignature
tempoSignature = buildTempoSignature <$> maybe quotedString <*> many fraction <*> char '=' <*> int <*> maybe quotedString

sharpOrFlat : Parser Accidental
sharpOrFlat = 
   map  (\x -> if x == '#' then Sharp else Flat)          
           (choice [ char '#', char 'b'])

keyName : Parser String
keyName = regex "[A-G]"

keySignature : Parser KeySignature
keySignature = (,,) <$> keyName <*> maybe sharpOrFlat <*> maybe mode

mode : Parser Mode
mode = choice 
         [ major
         , minor
         , ionian
         , dorian
         , phrygian
         , lydian
         , mixolydian
         , aeolian
         , locrian
         ]


minor : Parser Mode
minor = succeed Minor <* whiteSpace <* regex "(M|m)(I|i)(N|n)([A-Za-z])*"

major : Parser Mode
major = succeed Major <* whiteSpace <* regex "(M|m)(A|a)(J|j)([A-Za-z])*"

ionian : Parser Mode
ionian = succeed Ionian <* whiteSpace <* regex "(I|i)(O|o)(N|n)([A-Za-z])*"

dorian : Parser Mode
dorian = succeed Dorian <* whiteSpace <* regex "(D|d)(O|o)(R|r)([A-Za-z])*"

phrygian : Parser Mode
phrygian = succeed Phrygian <* whiteSpace <* regex "(P|p)(H|h)(R|r)([A-Za-z])*"

lydian : Parser Mode
lydian = succeed Lydian <* whiteSpace <* regex "(L|l)(Y|y)(D|d)([A-Za-z])*"

mixolydian : Parser Mode
mixolydian = succeed Mixolydian <* whiteSpace <* regex "(M|m)(I|i)(X|x)([A-Za-z])*"
 
aeolian : Parser Mode
aeolian = succeed Aeolian <* whiteSpace <* regex "(A|a)(E|e)(O|o)([A-Za-z])*"

locrian : Parser Mode
locrian = succeed Locrian <* whiteSpace <* regex "(L|l)(O|o)(C|c)([A-Za-z])*"
 
 
-- Headers
area : Parser Header
area = Area <$> ((headerCode 'A') *> strToEol)

book : Parser Header
book = Book <$> ((headerCode 'B') *> strToEol)

composer : Parser Header
composer = Composer <$> ((headerCode 'C') *> strToEol)

discography : Parser Header
discography = Discography <$> ((headerCode 'D') *> strToEol)

fileUrl : Parser Header
fileUrl = FileUrl <$> ((headerCode 'F') *> strToEol)

group : Parser Header
group = Group <$> ((headerCode 'G') *> strToEol)

history : Parser Header
history = History <$> ((headerCode 'H') *> strToEol)

instruction : Parser Header
instruction = Instruction <$> ((headerCode 'I') *> strToEol)

key : Parser Header
key = Key <$> ((headerCode 'K') *> keySignature <* eol)

unitNoteLength : Parser Header
unitNoteLength = UnitNoteLength <$> ((headerCode 'L') *> noteDuration <* eol)

meter : Parser Header
meter = Meter <$> ((headerCode 'M') *> meterSignature <* eol )

macro : Parser Header
macro = Macro <$> ((headerCode 'm') *> strToEol)

notes : Parser Header
notes = Notes <$> ((headerCode 'N') *> strToEol)

origin : Parser Header
origin = Origin <$> ((headerCode 'O') *> strToEol)

parts : Parser Header
parts = Parts <$> ((headerCode 'P') *> strToEol)

tempo : Parser Header
tempo = Tempo <$> ((headerCode 'Q') *> tempoSignature <* eol)

rhythm : Parser Header
rhythm = Rhythm <$> ((headerCode 'R') *> strToEol)

remark : Parser Header
remark = Remark <$> ((headerCode 'r') *> strToEol)

source : Parser Header
source = Source <$> ((headerCode 'S') *> strToEol)

symbolLine : Parser Header
symbolLine = SymbolLine <$> ((headerCode 's') *> strToEol)

title : Parser Header
title = Title <$> ((headerCode 'T') *> strToEol)

userDefined : Parser Header
userDefined = UserDefined <$> ((headerCode 'U') *> strToEol)

voice : Parser Header
voice = Voice <$> ((headerCode 'V') *> strToEol)


wordsAfter : Parser Header
wordsAfter  = WordsAfter  <$> ((headerCode 'W') *> strToEol)

wordsAligned : Parser Header
wordsAligned  = WordsAligned  <$> ((headerCode 'w') *> strToEol)

referenceNumber : Parser Header
referenceNumber = ReferenceNumber <$> ((headerCode 'X') *> intToEol)

transcription : Parser Header
transcription = Transcription <$> ((headerCode 'Z') *> strToEol)


header : Parser Header
header = 
  choice [ anywhereHeader
         , tuneHeader ]
           <?> "header"
           
tuneHeader : Parser Header
tuneHeader = 
  choice [ area 
         , book
         , composer
         , discography
         , fileUrl
         , group
         , history
         , origin
         , source
         , referenceNumber
         , transcription ]
           <?> "tune header"

anywhereHeader : Parser Header
anywhereHeader = 
  choice [ instruction 
         , key 
         , unitNoteLength 
         , meter
         , macro 
         , notes 
         , parts
         , tempo 
         , rhythm
         , remark 
         , title
         , userDefined
         , voice
         , wordsAfter ]
            <?> "anywhere header"

tuneBodyHeader : Parser Header
tuneBodyHeader = 
  choice [ symbolLine 
         , wordsAligned ] 
           <?> "tune body header"

headers : Parser TuneHeaders
headers = many1 header <?> "headers"

-- low level parsers
-- possible whitespace
whiteSpace : Parser String
whiteSpace = String.fromList <$> (many <| choice [space, tab])

-- at least one (intended) space somewhere inside the music body
spacer : Parser Music
spacer = Spacer <$> ( List.length <$> (many1 space))

endLine : Parser String
endLine = regex "(\r\n|\n)" 
            <?> "end line"
{- endLine = String.fromChar <$> choice [newline, crlf]
            <?> "end line"
-}

headerCode : Char -> Parser Char
headerCode c = char c <* char ':' <* whiteSpace

quotedString : Parser String
quotedString = 
   whiteSpace *> string "\"" *> regex "(\\\\\"|[^\"\n])*" <* string "\"" <* whiteSpace
      <?> "quoted string"

strToEol : Parser String
strToEol = String.fromList <$> many (noneOf [ '\r', '\n' ]) <* eol

intToEol : Parser Int
intToEol = int <* eol

-- temporary parsers - completely unfinished stuff
bodyStuff : Parser Music
bodyStuff = log "body stuff" <$> ( Stuff <$> (String.concat <$> (many1 <| regex "[^\r\n\t\\- A-Ga-g0-9]")))

anote : Parser Music
anote = buildNote <$> (regex "[A-Ga-g]") <*> maybe Combine.Num.digit

tuplet : Parser Music
tuplet = Tuplet <$> (char '(' *> Combine.Num.digit)


-- builders

-- build a fractional quantity - "x/y" -> (x,y)
buildFraction : Int -> Char -> Int -> MeterSignature
buildFraction x c y = (x,y)

-- build a tempo signature
buildTempoSignature : Maybe String -> List Fraction -> Char -> Int -> Maybe String -> TempoSignature
buildTempoSignature ms1 fs c i ms2 =
   let ms = 
      case ms1 of
        Nothing -> ms2
        _ -> ms1
   in
    { noteLengths = fs
    , bpm = i
    , marking = ms
    }
          
{- Build a Bar from an already parsed String.
   all guaranteed to be 2 char strings because of the parse pattern except the Iteration  
   which is a single digit string -}
buildBar : String -> Bar
buildBar s = case s of
   "||" -> DoubleBar False False
   "[|" -> DoubleBar True False
   "|]" -> DoubleBar False True
   "|:" -> RepeatStart
   ":|" -> RepeatEnd
   "::" -> StartAndEnd
   n -> 
     let 
       res = String.toInt n
     in case res of
        Ok i -> Iteration i
        _ -> SingleBar   -- (can't happen)

buildNote : String -> Maybe Int -> Music
buildNote s ml = 
   let 
     l = withDefault 1 ml
   in 
     Note s l

                
{- just for debug purposes - consume the rest of the input -}
rest : Parser (List Char)
rest = many anyChar

-- top level parsers
abc : Parser AbcTune
abc = (,) <$> headers <*> body 

          
-- exported functions

{-| entry point - Parse an ABC tune image -}
parse : String -> Result.Result String AbcTune
parse s =
  -- case Combine.parse midi s of 
  case Combine.parse (abc <* rest) s of
    (Ok n, _) ->
      Ok n

    (Err ms, cx) ->
      Err ("parse error: " ++ (toString ms) ++ ", " ++ (toString cx))




        
