module Abc
    (  parse
    ) where

{-|  Library for parsing ABC transcriptions using parser combinators
     see http://abcnotation.com/wiki/abc:standard:v2.1

# Definition

# Functions
@docs parse

-}

import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Infix exposing (..)
import Combine.Num exposing (..)
import Ratio exposing (Rational, over, fromInt)
import String exposing (fromList, toList, foldl)
import Char exposing (fromCode, toCode, isUpper)
import Debug exposing (..)
import Maybe exposing (withDefault)
import Maybe.Extra exposing (join)
import Result exposing (Result)
import Abc.ParseTree exposing (..)


-- top level parsers
abc : Parser AbcTune
abc = (,) <$> headers <*> many1 body <* restOfInput

-- BODY
body : Parser BodyPart
body = choice 
         [ score
         , tuneBodyHeader
         ]

score : Parser BodyPart 
score = Score <$> musicLine <*> endLine 

musicLine : Parser (List Music)
musicLine = many musicItem

musicItem : Parser Music
musicItem = rec <| \() -> 
    log "music item" <$>
    (
    choice 
       [ 
         chord               -- I think we need it placed before barline because of ambiguity of '['
       , inline              -- ditto
       , barline
       , brokenRhythmPair    -- must place before note because of potential ambiguity of AbcNote
       , note
       , rest
       , tuplet
       , slur
       , graceNote           -- we are not enforcing the ordering of grace notes, chords etc pre-note
       , chordSymbol
       , decoration
       , spacer
       ]       
    )

{- a bar line (plus optional repeat iteration marker)
   see comments in 4.8 Repeat/bar symbols:
   Abc parsers should be quite liberal in recognizing bar lines. In the wild, bar lines may have 
   any shape, using a sequence of | (thin bar line), [ or ] (thick bar line), and : (dots), e.g. |[| or [|::: 
-}
barline : Parser Music
barline = buildBarline <$> barSeparator <*> maybe Combine.Num.digit
             <?> "barline"

barSeparator : Parser String
barSeparator = 
  String.fromList <$>
    (
    many1 <|
      choice 
        [ char '|'
        , char '['
        , char ']'
        , char ':'
        ]
    )       

slur : Parser Music
slur = Slur <$> choice [char '(', char ')']
             <?> "slur"      

{- Note, Slur should really be defined as Slur (List Music) and then parsed as shown below.  This would allow slurs to be
   nested and the parser to test that the brackets are balanced.  However, unfortunately, in the wild there are examples
   of slurs that go across music lines which make this interpretation impossible.  We thus simply parse the bracket characters.

   lazy evaluation of this unimplemented slur - comments courtesy of Bogdanp.
   Elm is eagerly evaluated so slur and slurContent end up creating a circular dependency that gets evaluated as soon as the output JS does, 
   which means one function ends up calling the constructor of the other before the other is defined. The rec combinator introduces
   laziness to get round the problem


slur : Parser Music
slur = rec <| \() -> 
        Slur <$>  parens (many1 musicItem)
             <?> "slur"
-}


brokenRhythmTie : Parser String
brokenRhythmTie  = regex "(<+|>+)"

brokenRhythmPair : Parser Music
brokenRhythmPair = BrokenRhythmPair <$> abcNote <*> brokenRhythmTie <*> abcNote
             <?> "broken rhythm pair"

rest : Parser Music
rest = Rest <$> (withDefault (fromInt 1) <$> (regex "[XxZz]" *> maybe noteDur))
             <?> "rest"
      

{- a free - format chord symbol - see 4.18 Chord symbols -}
chordSymbol : Parser Music
chordSymbol = ChordSymbol <$> quotedString

chord : Parser Music
chord = Chord <$> 
         between (char '[') (char ']') (many1 abcNote)

inline : Parser Music
inline = Inline <$> 
           between (char '[') (char ']') tuneBodyInfo

graceNote : Parser Music
graceNote =  between (char '{') (char '}') grace
               <?> "grace note"

grace : Parser Music
grace = GraceNote <$> acciaccatura <*> choice [noteSequence, chord]

{- acciaccaturas are indicated with an optional forward slash -}
acciaccatura : Parser Bool
-- acciaccatura = withDefault False <$> ( (\_ -> True) <$> maybe (char '/'))
acciaccatura = (\_ -> True) <$> maybe (char '/')

decoration : Parser Music
decoration = Decoration <$> choice [shortDecoration, longDecoration]

shortDecoration : Parser String
shortDecoration = regex "[\\.~HLMOPSTuv]"

longDecoration : Parser String
longDecoration =  between (char '!') (char '!') (regex "[^\r\n!]*")
               <?> "long decoration"

{- just a sequence of Notes -}
noteSequence : Parser Music
noteSequence = NoteSequence <$> many1 note

-- general attributes
-- e.g 3/4
rational : Parser Rational
rational = Ratio.over <$> int <* char '/' <*> int



-- e.g. /4 (as found in note durations)
curtailedRational : Parser Rational
curtailedRational = Ratio.over 1 <$> (char '/' *> int)

{- e.g. / or // or /// (as found in note durations)
   which translates to 1/2, 1/4, 1/8 etc
-}
slashesRational : Parser Rational
slashesRational = 
   buildRationalFromExponential <$> (List.length <$> (many1 <| char '/'))


-- HEADER ATTRIBUTES

-- rational with trailing optional spaces
headerRational : Parser Rational
headerRational = rational <* whiteSpace

{- experimental -}
meterDefinition : Parser MeterSignature
meterDefinition =
  choice 
   [
     cutTime
   , commonTime
   , meterSignature
   ]

commonTime : Parser MeterSignature
commonTime = succeed (4,4) <* char 'C'

cutTime : Parser MeterSignature
cutTime = succeed (2,2) <* string "C|"

-- can't use Rationals for these because they cancel
meterSignature : Parser MeterSignature
meterSignature = (,) <$> int <* char '/' <*> int <* whiteSpace

{-
meterSignature : Parser MeterSignature
meterSignature = rational <* whiteSpace
-}

noteDuration : Parser NoteDuration
noteDuration = rational <* whiteSpace

{- This is an area where we've relaxed the 2.1 spec
   1) we use many rather than many1 note length designators
   2) equals is now optional
   This means we can parse all of
        1/4 3/8 1/4 3/8=40
        "Allegro" 1/4=120
        3/8=50 "Slowly"
   but also the outdated (but prevalent in the wild
   120  (which means 1/4=120)
-}
tempoSignature : Parser TempoSignature
tempoSignature = buildTempoSignature <$> maybe quotedString <*> many headerRational <*> maybe (char '=') <*> int <*> maybe quotedString

-- accidental in a key signature (these use a different representation from accidentals in the tune body
sharpOrFlat : Parser Accidental
sharpOrFlat = 
   map  (\x -> if x == '#' then Sharp else Flat)          
           (choice [ char '#', char 'b'])

keyName : Parser String
keyName = regex "[A-G]"

keySignature : Parser KeySignature
keySignature = buildKeySignature <$> keyName <*> maybe sharpOrFlat <*> maybe mode

mode : Parser Mode
mode = choice 
         [ major
         , ionian
         , dorian
         , phrygian
         , lydian
         , mixolydian
         , aeolian
         , locrian
         , minor   -- place last because of potential ambiguity	
         ]


minor : Parser Mode
-- minor = succeed Minor <* whiteSpace <* regex "(M|m)(I|i)(N|n)([A-Za-z])*"
minor = succeed Minor <* whiteSpace <* regex "(M|m)([A-Za-z])*"

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
instruction = Instruction <$> ((headerCode 'I') *> inlineInfo )

key : Parser Header
key = Key <$> ((headerCode 'K') *> keySignature )

unitNoteLength : Parser Header
unitNoteLength = UnitNoteLength <$> ((headerCode 'L') *> noteDuration )

meter : Parser Header
meter = Meter <$> ((headerCode 'M') *> meterDefinition  )

macro : Parser Header
macro = Macro <$> ((headerCode 'm') *> inlineInfo)

notes : Parser Header
notes = Notes <$> ((headerCode 'N') *> inlineInfo)

origin : Parser Header
origin = Origin <$> ((headerCode 'O') *> strToEol)

parts : Parser Header
parts = Parts <$> ((headerCode 'P') *> inlineInfo)

tempo : Parser Header
tempo = Tempo <$> ((headerCode 'Q') *> tempoSignature)

rhythm : Parser Header
rhythm = Rhythm <$> ((headerCode 'R') *> inlineInfo)

remark : Parser Header
remark = Remark <$> ((headerCode 'r') *> inlineInfo)

source : Parser Header
source = Source <$> ((headerCode 'S') *> strToEol)

symbolLine : Parser Header
symbolLine = SymbolLine <$> ((headerCode 's') *> inlineInfo)

title : Parser Header
title = Title <$> ((headerCode 'T') *> inlineInfo)

userDefined : Parser Header
userDefined = UserDefined <$> ((headerCode 'U') *> inlineInfo)

voice : Parser Header
voice = Voice <$> ((headerCode 'V') *> inlineInfo)

wordsAfter : Parser Header
wordsAfter  = WordsAfter  <$> ((headerCode 'W') *> inlineInfo)

wordsAligned : Parser Header
wordsAligned  = WordsAligned  <$> ((headerCode 'w') *> inlineInfo)

referenceNumber : Parser Header
referenceNumber = ReferenceNumber <$> ((headerCode 'X') *> int)

transcription : Parser Header
transcription = Transcription <$> ((headerCode 'Z') *> strToEol)

{- a header is an information field up to and including the end of line marker -}
header : Parser Header
header = informationField <* eol

{- ditto for headers that may appear in the tune body -}
tuneBodyHeader : Parser BodyPart
tuneBodyHeader  = BodyInfo <$> tuneBodyInfo <* eol

{- whereas information fields can be used inline -}
informationField : Parser Header
informationField = 
  log "header" <$>
    (
    choice [ anywhereInfo
           , tuneInfo ]
             <?> "header"
    )
           
tuneInfo : Parser Header
tuneInfo = 
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
           <?> "tune info"

anywhereInfo : Parser Header
anywhereInfo = 
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
         , wordsAfter
         , comment
         ]
            <?> "anywhere info"

tuneBodyOnlyInfo : Parser Header
tuneBodyOnlyInfo = 
  choice [ symbolLine 
         , wordsAligned ] 
           <?> "tune body only infp"

tuneBodyInfo : Parser Header
tuneBodyInfo = 
  choice [ tuneBodyOnlyInfo
         , anywhereInfo ] 
           <?> "tune body info"

{- relax the spec in the pasring of headers to allow body-only tunes -}
headers : Parser TuneHeaders
headers = many header <?> "headers"
-- headers = many1 header <?> "headers"

{- comments.  These are introduced with '%' and can occur anywhere.
   The stylesheet directive '%%' is not recognized here and will
   simply be treated as a comment.  We'll treat comments as Headers
   so as not to pollute the parse tree overmuch
-}
comment : Parser Header
comment = Comment <$> (regex "%" *> strToEol)

-- low level parsers
-- possible whitespace
whiteSpace : Parser String
whiteSpace = String.fromList <$> (many <| choice [space, tab])

-- at least one (intended) space somewhere inside the music body
spacer : Parser Music
spacer = Spacer <$> ( List.length <$> (many1 space))

{- this is an area where the spec is uncertain.  See 6.1.1 Typesetting line-breaks
   The forward slash is used to indicate 'continuation of input lines' often because
   users may need to avoid long lines if, for example, they would otherwise extend
   beyond the limit of an old email system.  All very out of date, but nevertheless
   still prevalent in the wild.  We take the view that we must do our best to recognise 
   them and then throw them away (along with any other later stuff in the line)

   Return True if we have a continuation
-}
continuation : Parser Bool
continuation = succeed True <* char '\\' <* regex "[^\r\n]*"

endLine : Parser Bool
endLine = withDefault False <$> maybe continuation <* regex "(\r\n|\n|\r)" 
            <?> "end line"


headerCode : Char -> Parser Char
headerCode c = char c <* char ':' <* whiteSpace

quotedString : Parser String
quotedString = 
   whiteSpace *> string "\"" *> regex "(\\\\\"|[^\"\n])*" <* string "\"" <* whiteSpace
      <?> "quoted string"

{- parse a remaining string up to but not including the end of line -}
strToEol : Parser String
-- strToEol = String.fromList <$> many (noneOf [ '\r', '\n' ]) 
strToEol = regex "[^\r\n]*"

{- parse an information item String - note that, because these can be used inline
   (bracketed by '[' and ']') it behoves us not to use the framing characters in the string
   not that the spec has anything to say about it as far as I can see
-}
inlineInfo : Parser String
inlineInfo = regex "[^\r\n\\[\\]]*"

note : Parser Music
note = Note <$> abcNote

abcNote : Parser AbcNote
abcNote = buildNote <$> maybeAccidental <*> keyClass <*> moveOctave <*> maybe noteDur <*> maybeTie

{- an upper or lower case note ([A-Ga-g]) 
   done this way rather than a regex to get a more tractable Char result and not a String
-}
keyClass : Parser Char
keyClass =
  let
    f a = (toCode a >= 65 && toCode a <= 71)
          || (toCode a >= 97 && toCode a <= 103)
  in 
    satisfy f 

-- maybe an accidental defining a note's pitch
maybeAccidental : Parser (Maybe String)
maybeAccidental = 
  maybe <|
      choice
        [ string "^^"
        , string "__"
        , string "^"
        , string "_"
        , string "="
        ]
 

-- move an octave - altering a note's pitch (up or down) by a succession of octaves
{- old implementation
moveOctave : Parser Int
moveOctave = 
   withDefault 0 <$>
     maybe 
       (choice
          [ octave True  -- up
          , octave False  -- down
          ]
        )
-}

{- True means octave up (denoted by an apostrophe
   False means octave down (denoted by a comma)
   return a positive or negative number according to the number of markers parsed
-}
{- old implementation
octave : Bool -> Parser Int
octave b = 
   let 
     c =
       case b of 
         True -> '\''  -- up
         _ -> ','      -- down
     fn l = case b of  -- negate answers for low octaves
          True -> l
          _ -> 0 - l
   in
     log "octave" <$> (fn <$> (List.length <$> many1 (char c)))
-}

{- move an octave up (+ve - according to the number of apostrophes parsed)
             or down (-ve - according to the number of commas parsed)
-}
moveOctave : Parser Int
moveOctave =
   octaveShift <$> regex "[',]*"

{- count the number of apostrophe (up) or comma (down) characters in the string 
   and give the result a value of (up-down) 
-} 
octaveShift : String -> Int
octaveShift s =
  let 
    f c acc = case c of
      '\'' ->
        let 
          (up, down) = acc
        in
         (up+1, down)
      ',' ->
        let 
          (up, down) = acc
        in
         (up, down + 1)
      _ -> acc
    octs = String.foldl f (0,0) s
  in
    (fst octs - snd octs)

{- the duration of a note in the body -}
noteDur : Parser Rational
noteDur = 
   choice 
    [ integralAsRational
    , rational
    , curtailedRational
    , slashesRational
    ]

{- now attached to leading note and not free-standing -}
maybeTie : Parser (Maybe Char)
maybeTie = (maybe (char '-'))
         <?> "tie"


integralAsRational : Parser Rational
integralAsRational =
   Ratio.fromInt <$> Combine.Num.digit

tuplet : Parser Music
tuplet = Tuplet <$> (char '(' *> tupletSignature) <*> many1 abcNote

{- possible tuplet signatures
   (3             --> {3,2,3}
   (3:2           --> {3,2,3}
   (3::           --> {3,2,3}
   (3:2:4         --> {3,2,4}
   (3::2          --> {3,2,2}
-}
tupletSignature : Parser TupletSignature
tupletSignature = buildTupletSignature <$> 
   regex "[2-9]" <*> tup <*> tup

tup : Parser (Maybe String)
tup = join <$> maybe 
        (char ':' *> maybe (regex "[2-9]"))

-- flip the first 2 arguments of a 3-argument function
{-
flip2of3 : (a -> b -> c -> d ) -> (b -> a -> c -> d)  
flip2of3 f = 
    let 
      g x y z = f y x z
    in
      g
-}




-- builders

-- build a rationalal quantity - "x/y" -> Rational x y
{-
buildRational : Int -> Char -> Int -> MeterSignature
buildRational x slash y = x `over` y
-}

{- used in counting slashes exponentially -}
buildRationalFromExponential : Int -> Rational
buildRationalFromExponential i =
  Ratio.over 1 (2 ^ i)

-- build a tempo signature
buildTempoSignature : Maybe String -> List Rational -> Maybe Char -> Int -> Maybe String -> TempoSignature
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

-- build a key signature
buildKeySignature : String -> Maybe Accidental -> Maybe Mode -> KeySignature
buildKeySignature kc ma mm =
  { keyClass = kc, accidental = ma, mode = mm }

{- build a bar line (i.e. a separation  between bars) -}
buildBarline : String -> Maybe Int -> Music
buildBarline s i = Barline { separator = s, iteration = i }
          
buildNote : Maybe String -> Char -> Int -> Maybe Rational -> Maybe Char -> AbcNote
buildNote macc c octave ml mt = 
   let 
     l = withDefault (Ratio.fromInt 1) ml
     a = buildAccidental macc
     spn = scientificPitchNotation c octave
     tied = case mt of
        Just _ -> True
        _ -> False
   in 
     { pitchClass = c, accidental = a, octave = spn, duration = l, tied = tied }

{- investigate a note/octave pair and return the octave
   in scientific pitch notation (middle C = 4)
-}
scientificPitchNotation : Char -> Int -> Int
scientificPitchNotation keyClass oct =
   if isUpper keyClass then  -- key class inhabits octave of middle C, oct <= 0
      4 + oct
   else                      -- key class inhabits octave above middle C, oct >= 0
      5 + oct

buildAccidental : Maybe String -> Maybe Accidental
buildAccidental ms = case ms of
   Just "^^" -> Just DoubleSharp
   Just "__" -> Just DoubleFlat
   Just "^"  -> Just Sharp
   Just "_"  -> Just Flat
   Just "="  -> Just Natural
   _ -> Nothing 

{- build a tuplet signature {p,q,r) - p notes in the time taken for q
   in operation over the next r notes
-}
buildTupletSignature : String -> Maybe String -> Maybe String -> TupletSignature
buildTupletSignature ps mq mr = 
  let 
    p = toTupletInt ps

    -- default values for q.  Not quite in accordance with spec where q varies
    -- between 2 and 3 for odd values of p, dependent on the time signature
    -- (but this would make the parser stateful which we don't want for such small
    -- edge cases)
    qdefault = case p of
      2 -> 3
      3 -> 2
      4 -> 3
      6 -> 2
      8 -> 3
      _ -> 2   
    q = withDefault qdefault (Maybe.map toTupletInt mq)
    r = withDefault p (Maybe.map toTupletInt mr)
  in
    (p,q,r)

toTupletInt : String -> Int
toTupletInt s =
  s |> String.toInt
    |> Result.toMaybe
    |> withDefault 3   -- default can't happen because all strings are regex-parsed 2-9    

                
{- just for debug purposes - consume the rest of the input -}
restOfInput : Parser (List Char)
restOfInput = many anyChar
          
-- exported functions

{-| entry point - Parse an ABC tune image -}
parse : String -> Result.Result String AbcTune
parse s =
  -- case Combine.parse midi s of 
  case Combine.parse (abc <* restOfInput) s of
    (Ok n, _) ->
      Ok n

    (Err ms, cx) ->
      Err ("parse error: " ++ (toString ms) ++ ", " ++ (toString cx))




        
