module Test.Music (tests) where

import ElmTest exposing (..)
import Music.Notation exposing (..)
import Abc.ParseTree exposing (PitchClass(..), KeySignature, Accidental(..), Mode(..), AbcNote)
import Maybe exposing (Maybe)
import Ratio exposing (Rational, over, fromInt)

import String

equivalentKeys : List KeyClass -> List KeyClass -> Bool
equivalentKeys actual expected =
  let
    f key acc = acc && (List.member key expected)
  in
    (List.foldl f True actual) && (List.length actual == List.length expected)

fNatural : AbcNote
fNatural = { pitchClass = F,  accidental = Nothing, octave = 4, duration = fromInt 1, tied = False }

gMajor : KeySignature
gMajor = { pitchClass = G, accidental = Nothing, mode = Major }

cMajor : KeySignature
cMajor = { pitchClass = C, accidental = Nothing, mode = Major }

tests : Test
tests =
  let 
    majorMode =
      suite "Major mode"
        [ test "G Major" (assert <| equivalentKeys 
             (keySet { pitchClass = G, accidental = Nothing, mode = Major }) 
             [ (F, Just Sharp) ]
             )      
        , test "Ab Major" (assert <| equivalentKeys 
             (keySet { pitchClass = A, accidental = Just Flat, mode = Major })
             [ (B, Just Flat), (E, Just Flat), (A, Just Flat), (D, Just Flat) ]
             )      
        , test "A Major" (assert <| equivalentKeys 
             (keySet { pitchClass = A, accidental = Nothing, mode = Major }) 
             [ (C, Just Sharp), (F, Just Sharp), (G, Just Sharp)]
             )       
        , test "Bb Major" (assert <| equivalentKeys 
             (keySet { pitchClass = B, accidental = Just Flat, mode = Major })
             [ (B, Just Flat), (E, Just Flat) ]
             )      
        , test "B Major" (assert <| equivalentKeys 
             (keySet { pitchClass = B, accidental = Nothing, mode = Major }) 
             [ (C, Just Sharp), (F, Just Sharp), (G, Just Sharp), (D, Just Sharp), (A, Just Sharp)]
             )     
        , test "C Major" (assert <| List.isEmpty
             (keySet { pitchClass = C, accidental = Nothing, mode = Major})
             )       
        , test "Db Major" (assert <| equivalentKeys 
             (keySet { pitchClass = D, accidental = Just Flat, mode = Major }) 
             [ (B, Just Flat), (E, Just Flat), (A, Just Flat), (D, Just Flat), (G, Just Flat)]
             )       
        , test "D Major" (assert <| equivalentKeys 
             (keySet { pitchClass = D, accidental = Nothing, mode = Major })
             [ (F, Just Sharp), (C, Just Sharp) ]
             )     
        , test "Eb Major" (assert <| equivalentKeys 
             (keySet { pitchClass = E, accidental = Just Flat, mode = Major })
             [ (B, Just Flat), (E, Just Flat), (A, Just Flat)  ]
             )        
        , test "E Major" (assert <| equivalentKeys 
             (keySet { pitchClass = E, accidental = Nothing, mode = Major }) 
             [ (C, Just Sharp), (F, Just Sharp), (G, Just Sharp), (D, Just Sharp)]
             )            
        , test "F Major" (assert <| equivalentKeys 
             (keySet { pitchClass = F, accidental = Nothing, mode = Major })
             [ (B, Just Flat) ]
             )       
        , test "F# Major" (assert <| equivalentKeys 
             (keySet { pitchClass = F, accidental = Just Sharp, mode = Major }) 
             [ (C, Just Sharp), (F, Just Sharp), (G, Just Sharp), (D, Just Sharp), (A, Just Sharp), (E, Just Sharp)]
             )           
        , test "Gb Major" (assert <| equivalentKeys 
             (keySet { pitchClass = G, accidental = Just Flat, mode = Major }) 
             [ (B, Just Flat), (E, Just Flat), (A, Just Flat), (D, Just Flat), (G, Just Flat), (C, Just Flat)]
             )       
        ]    
    minorMode =
      suite "Minor mode"
        [ test "G Minor" (assert <| equivalentKeys 
             (keySet { pitchClass = G, accidental = Nothing, mode = Minor }) 
             [ (B, Just Flat), (E, Just Flat) ]
             )        
        , test "A Minor" (assert <| List.isEmpty
             (keySet { pitchClass = A, accidental = Nothing, mode = Minor})
             )       
        ]  
    klezmerMode = 
      suite "Klezmer mode"
        [ test "D Phrygian with sharpened f" (assert <| equivalentKeys 
             (modifiedKeySet ({ pitchClass = D, accidental = Nothing, mode = Phrygian }, [{ pitchClass = F, accidental = Sharp}]) ) 
             [ (B, Just Flat), (E, Just Flat), (F, Just Sharp) ]
             )   
        ]  
    otherModes =
      suite "Other mode"
        [ test "C Doriam" (assert <| equivalentKeys 
             (keySet { pitchClass = C, accidental = Nothing, mode = Dorian }) 
             [ (B, Just Flat), (E, Just Flat) ]
             )        
        , test "D Dorian" (assert <| List.isEmpty
             (keySet { pitchClass = D, accidental = Nothing, mode = Dorian})
             )     
        , test "C Phrygian" (assert <| equivalentKeys 
             (keySet { pitchClass = C, accidental = Nothing, mode = Phrygian }) 
             [ (B, Just Flat), (E, Just Flat), (A, Just Flat), (D, Just Flat) ]
             )              
        , test "E Phrygian" (assert <| List.isEmpty
             (keySet { pitchClass = E, accidental = Nothing, mode = Phrygian})
             )       
        , test "C Lydian" (assert <| equivalentKeys 
             (keySet { pitchClass = C, accidental = Nothing, mode = Lydian }) 
             [ (F, Just Sharp) ]
             )             
        , test "F Lydian" (assert <| List.isEmpty
             (keySet { pitchClass = F, accidental = Nothing, mode = Lydian})
             )         
        , test "C Mixolyydian" (assert <| equivalentKeys 
             (keySet { pitchClass = C, accidental = Nothing, mode = Mixolydian }) 
             [ (B, Just Flat) ]
             )                 
        , test "G Lydian" (assert <| List.isEmpty
             (keySet { pitchClass = G, accidental = Nothing, mode = Mixolydian})
             )             
        , test "C Aeolian" (assert <| equivalentKeys 
             (keySet { pitchClass = C, accidental = Nothing, mode = Aeolian }) 
             [ (B, Just Flat), (E, Just Flat), (A, Just Flat)  ]
             )                        
        , test "A Aeolian" (assert <| List.isEmpty
             (keySet { pitchClass = A, accidental = Nothing, mode = Aeolian})
             )                
        , test "C Locrian" (assert <| equivalentKeys 
             (keySet { pitchClass = C, accidental = Nothing, mode = Locrian }) 
             [ (B, Just Flat), (E, Just Flat), (A, Just Flat), (D, Just Flat), (G, Just Flat)]
             )                           
        , test "B Locrian" (assert <| List.isEmpty
             (keySet { pitchClass = B, accidental = Nothing, mode = Locrian})
             )        
        , test "G Ionian" (assert <| equivalentKeys 
             (keySet { pitchClass = G, accidental = Nothing, mode = Ionian }) 
             [ (F, Just Sharp) ]
             )       
        , test "C Ionian" (assert <| List.isEmpty
             (keySet { pitchClass = C, accidental = Nothing, mode = Major})
             )                  
        ]   
    lookups =
      suite "lookups"
        [ test "f in G Major" (assertEqual
               (Just Sharp)
               (accidentalImplicitInKey fNatural gMajor)
               )
        , test "f in C Major" (assertEqual
               (Nothing)
               (accidentalImplicitInKey fNatural cMajor)
               )
        ]
    in
      suite "Music Notation"
          [ majorMode
          , minorMode
          , klezmerMode
          , otherModes
          , lookups
          ]
