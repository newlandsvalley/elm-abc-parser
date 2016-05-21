module Test.Music exposing 
  (tests) 

import ElmTest exposing (..)
import Music.Notation exposing (..)
import Abc.ParseTree exposing (PitchClass(..), KeySignature, Accidental(..), KeyAccidental, Mode(..), AbcNote)
import Ratio exposing (Rational, over, fromInt)

import String

equivalentKeys : List KeyAccidental -> List KeyAccidental -> Bool
equivalentKeys actual expected =
  let
    f key acc = acc && (List.member key expected)
  in
    (List.foldl f True actual) && (List.length actual == List.length expected)

fNatural : AbcNote
fNatural = { pitchClass = F,  accidental = Nothing, octave = 4, duration = fromInt 1, tied = False }

gMajor : KeySignature
gMajor = { pitchClass = G, accidental = Nothing, mode = Major }

gMinor : KeySignature
gMinor = { pitchClass = G, accidental = Nothing, mode = Minor }

cMajor : KeySignature
cMajor = { pitchClass = C, accidental = Nothing, mode = Major }

dMajor : KeySignature
dMajor = { pitchClass = D, accidental = Nothing, mode = Major }

fMajor : KeySignature
fMajor = { pitchClass = F, accidental = Nothing, mode = Major }

tests : Test
tests =
  let 
    majorMode =
      suite "Major mode"
        [ test "G Major" (assert <| equivalentKeys 
             (keySet { pitchClass = G, accidental = Nothing, mode = Major }) 
             [ (F, Sharp) ]
             )      
        , test "Ab Major" (assert <| equivalentKeys 
             (keySet { pitchClass = A, accidental = Just Flat, mode = Major })
             [ (B, Flat), (E, Flat), (A, Flat), (D, Flat) ]
             )      
        , test "A Major" (assert <| equivalentKeys 
             (keySet { pitchClass = A, accidental = Nothing, mode = Major }) 
             [ (C, Sharp), (F, Sharp), (G, Sharp)]
             )       
        , test "Bb Major" (assert <| equivalentKeys 
             (keySet { pitchClass = B, accidental = Just Flat, mode = Major })
             [ (B, Flat), (E, Flat) ]
             )      
        , test "B Major" (assert <| equivalentKeys 
             (keySet { pitchClass = B, accidental = Nothing, mode = Major }) 
             [ (C, Sharp), (F, Sharp), (G, Sharp), (D, Sharp), (A, Sharp)]
             )     
        , test "C Major" (assert <| List.isEmpty
             (keySet { pitchClass = C, accidental = Nothing, mode = Major})
             )       
        , test "Db Major" (assert <| equivalentKeys 
             (keySet { pitchClass = D, accidental = Just Flat, mode = Major }) 
             [ (B, Flat), (E, Flat), (A, Flat), (D, Flat), (G, Flat)]
             )       
        , test "D Major" (assert <| equivalentKeys 
             (keySet { pitchClass = D, accidental = Nothing, mode = Major })
             [ (F, Sharp), (C, Sharp) ]
             )     
        , test "Eb Major" (assert <| equivalentKeys 
             (keySet { pitchClass = E, accidental = Just Flat, mode = Major })
             [ (B, Flat), (E, Flat), (A, Flat)  ]
             )        
        , test "E Major" (assert <| equivalentKeys 
             (keySet { pitchClass = E, accidental = Nothing, mode = Major }) 
             [ (C, Sharp), (F, Sharp), (G, Sharp), (D, Sharp)]
             )            
        , test "F Major" (assert <| equivalentKeys 
             (keySet { pitchClass = F, accidental = Nothing, mode = Major })
             [ (B, Flat) ]
             )       
        , test "F# Major" (assert <| equivalentKeys 
             (keySet { pitchClass = F, accidental = Just Sharp, mode = Major }) 
             [ (C, Sharp), (F, Sharp), (G, Sharp), (D, Sharp), (A, Sharp), (E, Sharp)]
             )           
        , test "Gb Major" (assert <| equivalentKeys 
             (keySet { pitchClass = G, accidental = Just Flat, mode = Major }) 
             [ (B, Flat), (E, Flat), (A, Flat), (D, Flat), (G, Flat), (C, Flat)]
             )       
        ]    
    minorMode =
      suite "Minor mode"
        [ test "G Minor" (assert <| equivalentKeys 
             (keySet { pitchClass = G, accidental = Nothing, mode = Minor }) 
             [ (B, Flat), (E, Flat) ]
             )        
        , test "A Minor" (assert <| List.isEmpty
             (keySet { pitchClass = A, accidental = Nothing, mode = Minor})
             )       
        ]  
    klezmerMode = 
      suite "Klezmer mode"
        [ test "D Phrygian with sharpened f" (assert <| equivalentKeys 
             (modifiedKeySet ({ pitchClass = D, accidental = Nothing, mode = Phrygian }, [(F, Sharp)]) ) 
             [ (B, Flat), (E, Flat), (F, Sharp) ]
             )   
        ]  
    otherModes =
      suite "Other mode"
        [ test "C Doriam" (assert <| equivalentKeys 
             (keySet { pitchClass = C, accidental = Nothing, mode = Dorian }) 
             [ (B, Flat), (E, Flat) ]
             )        
        , test "D Dorian" (assert <| List.isEmpty
             (keySet { pitchClass = D, accidental = Nothing, mode = Dorian})
             )     
        , test "C Phrygian" (assert <| equivalentKeys 
             (keySet { pitchClass = C, accidental = Nothing, mode = Phrygian }) 
             [ (B, Flat), (E, Flat), (A, Flat), (D, Flat) ]
             )              
        , test "E Phrygian" (assert <| List.isEmpty
             (keySet { pitchClass = E, accidental = Nothing, mode = Phrygian})
             )       
        , test "C Lydian" (assert <| equivalentKeys 
             (keySet { pitchClass = C, accidental = Nothing, mode = Lydian }) 
             [ (F, Sharp) ]
             )             
        , test "F Lydian" (assert <| List.isEmpty
             (keySet { pitchClass = F, accidental = Nothing, mode = Lydian})
             )         
        , test "C Mixolyydian" (assert <| equivalentKeys 
             (keySet { pitchClass = C, accidental = Nothing, mode = Mixolydian }) 
             [ (B, Flat) ]
             )                 
        , test "G Lydian" (assert <| List.isEmpty
             (keySet { pitchClass = G, accidental = Nothing, mode = Mixolydian})
             )             
        , test "C Aeolian" (assert <| equivalentKeys 
             (keySet { pitchClass = C, accidental = Nothing, mode = Aeolian }) 
             [ (B, Flat), (E, Flat), (A, Flat)  ]
             )                        
        , test "A Aeolian" (assert <| List.isEmpty
             (keySet { pitchClass = A, accidental = Nothing, mode = Aeolian})
             )                
        , test "C Locrian" (assert <| equivalentKeys 
             (keySet { pitchClass = C, accidental = Nothing, mode = Locrian }) 
             [ (B, Flat), (E, Flat), (A, Flat), (D, Flat), (G, Flat)]
             )                           
        , test "B Locrian" (assert <| List.isEmpty
             (keySet { pitchClass = B, accidental = Nothing, mode = Locrian})
             )        
        , test "G Ionian" (assert <| equivalentKeys 
             (keySet { pitchClass = G, accidental = Nothing, mode = Ionian }) 
             [ (F, Sharp) ]
             )       
        , test "C Ionian" (assert <| List.isEmpty
             (keySet { pitchClass = C, accidental = Nothing, mode = Major})
             )                  
        ]   
    lookups =
      suite "lookups"
        [ test "f in G Major" (assertEqual
               (Just Sharp)
               (accidentalImplicitInKey F (gMajor, []))
               )
        , test "f in C Major" (assertEqual
               (Nothing)
               (accidentalImplicitInKey F (cMajor, []))
               )

        ]   
    keys =
      suite "keys"
        [ test "D is a sharp key" (assert 
                 (isCOrSharpKey dMajor)
               )
        , test "C is an (honourary) sharp key" (assert 
                 (isCOrSharpKey cMajor)
               )
        , test "F is not a sharp key" (assert 
                 (not (isCOrSharpKey fMajor))
               )
        , test "Gm is not a sharp key" (assert 
                 (not (isCOrSharpKey gMinor))
               )
        ]   
    in
      suite "Music Notation"
        [ majorMode
        , minorMode
        , klezmerMode
        , otherModes
        , lookups
        , keys
        ]


