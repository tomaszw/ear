{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, FlexibleInstances #-}
module Pitch where

import Data.Word
import Data.Maybe
import Data.Char

class Pitched a where
  pitchValue :: a -> Word8
  toPitch :: Word8 -> a

data Pitch = forall a. (Pitched a) => Pitch a

instance Pitched Pitch where
  pitchValue (Pitch x) = pitchValue x
  toPitch v = Pitch v

instance Eq Pitch where
  a == b = pitchValue a == pitchValue b

instance Show Pitch where
  show = pitchStr

instance Pitched Word8 where
  pitchValue x = x
  toPitch x = x

class Distance a where
  semitones :: a -> Int

instance Distance Int where semitones = id
instance Distance Integer where semitones = fromIntegral . id

convPitch :: (Pitched a, Pitched b) => a -> b
convPitch x = toPitch (pitchValue x)

pitchToOctDegree :: Pitched a => a -> (Int, Int)
pitchToOctDegree p =
  let oct = (pitchValue p - pitchValue pitchC0) `div` 12
      deg = (pitchValue p - pitchValue pitchC0) `mod` 12
  in (fromIntegral oct, fromIntegral deg)

pitchFromOctDegree :: Pitched a => (Int, Int) -> a
pitchFromOctDegree (oct, deg) =
  toPitch . pitchValue $ pitchC0 `transpose` (oct * 12 + deg)

changeOctave :: Pitched a => a -> Int -> a
changeOctave p oct =
  let (_, deg) = pitchToOctDegree p in
  pitchFromOctDegree (oct, deg)

transpose :: (Pitched a, Distance d) => a -> d -> a
transpose p v = toPitch $ pitchValue p + fromIntegral (semitones v)

pitchRange :: (Pitched a) => a -> a -> [a]
pitchRange a b = map toPitch [pitchValue a .. pitchValue b]

pitchC0, pitchC1, pitchC2, pitchC3, pitchC4, pitchC5, pitchC6, pitchC7 :: Pitch

pitchC0 = toPitch 12
pitchC1 = toPitch 24
pitchC2 = toPitch 36
pitchC3 = toPitch 48
pitchC4 = toPitch 60
pitchC5 = toPitch 72
pitchC6 = toPitch 84
pitchC7 = toPitch 96

pitchFromName :: Pitched a => String -> Maybe a
pitchFromName n = fmap f (noteFromName (map toUpper n)) where
  f note = convPitch $ P note oct
  isNameChar c = isLetter c || c == '#'
  nameStr = takeWhile isNameChar n
  octStr = takeWhile isDigit (dropWhile isNameChar n)
  oct = case octStr of
    "" -> 0
    s  -> read s
    
data NoteName = C | Csharp | D | Dsharp | E | F | Fsharp | G | Gsharp | A | Asharp | B
  deriving (Eq, Show, Enum)

noteNames = [(C, "C"), (Csharp, "C#"), (D, "D"), (Dsharp, "D#"), (E, "E"),
             (F, "F"), (Fsharp, "F#"), (G, "G"), (Gsharp, "G#"), (A, "A"),
             (Asharp, "A#"), (B, "B")]

noteName t = fromJust $ lookup t noteNames
noteFromName str = lookup str noteNames' where
  noteNames' = map (\(a,b) -> (b,a)) noteNames

pitchStr :: Pitched a => a -> String
pitchStr p = let (oct,deg) = pitchToOctDegree p in
  noteName (toEnum deg) ++ show oct
  
type Octave = Int
data NamedPitch = P NoteName Octave deriving Eq

instance Show NamedPitch where show = pitchStr
instance Pitched NamedPitch where
  pitchValue (P name oct) = pitchValue $ pitchC0 `transpose` (oct * 12 + fromEnum name)
  toPitch p = let (oct,deg) = pitchToOctDegree p in
    P (toEnum deg) oct
