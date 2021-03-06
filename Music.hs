{-# LANGUAGE ExistentialQuantification #-}
module Music where

import Data.Char
import Data.Word
import Data.Maybe
import Data.List (sortBy, maximum)
import Data.Ord

import Pitch

type Duration = Double
type Time = Double
newtype BPM = BPM Int deriving (Eq,Show)

data ToneRange
   = ToneRange { minOctave :: Int, maxOctave :: Int } deriving (Eq,Show)

data Tonality
   = Tonality {
     scale :: Scale
   , degrees :: [ScaleDegree]
   , root :: Pitch
   , range :: ToneRange
   } deriving (Eq, Show)

data Interval =
    Unison | Min2 | Maj2 | Min3 | Maj3 | Maj4
  | Aug4   | Maj5 | Min6 | Maj6 | Min7 | Maj7 | Oct
    deriving (Eq,Enum,Ord,Show)

instance Distance Interval where
  semitones = intervalSemitones

data Scale =
     Scale {
       scaleName :: String
     , scaleIntervals :: [Interval]
     , scaleSolfege_ :: Solfege
     } deriving (Eq, Show)

type ScaleDegree = Int
type Semitones = Int

type Solfege = [String]

data ChordQuality =
    ChMaj
  | ChMin
  | ChMaj7
  | ChMajD7
  | ChMinD7
  deriving (Eq, Show)

data Chord = Chord ChordQuality Pitch Int
             deriving (Eq, Show)

type Voice = [VoiceElement]
data VoiceElement =
    PitchE Pitch Duration
  | ChordE Chord Duration
  | SilenceE Duration
  | ParVoicesE [Voice]
  deriving (Eq, Show)
           
type NoteSeq = [(Pitch,Duration,Duration)]

chordIntervals :: ChordQuality -> [Interval]
chordIntervals ChMaj = [Unison, Maj3, Maj5]
chordIntervals ChMin = [Unison, Min3, Maj5]
chordIntervals ChMaj7 = [Unison, Maj3, Maj5, Maj7]
chordIntervals ChMajD7 = [Unison, Maj3, Maj5, Min7]
chordIntervals ChMinD7 = [Unison, Min3, Maj5, Min7]

chordPitches :: ChordQuality -> Pitch -> Int -> [Pitch]
chordPitches q root inversion =
  invert inversion $ map (transpose root . intervalSemitones) $ chordIntervals q
  where
    invert _ [] = []
    invert 0 ps = ps
    invert n (p:ps) | n > 0 = invert (n-1) $ ps ++ [p]
    invert n ps     | n < 0 = invert (n+1) $ head rev : reverse (tail rev)
                              where rev = reverse ps

voiceElemDuration :: VoiceElement -> Duration
voiceElemDuration (PitchE _ d) = d
voiceElemDuration (ChordE _ d) = d
voiceElemDuration (SilenceE d) = d
voiceElemDuration (ParVoicesE vs) = maximum (map voiceDuration vs)

voiceDuration :: Voice -> Duration
voiceDuration = sum . map voiceElemDuration

shiftI :: Pitch -> Interval -> Pitch
shiftI p i = p `transpose` intervalSemitones i

scaleLength :: Scale -> Int
scaleLength = length . scaleIntervals

normaliseDegree :: Scale -> ScaleDegree -> ScaleDegree
normaliseDegree s d =
  1 + ((d-1) `mod` (scaleLength s))

scalePitches :: Scale -> Pitch -> [Pitch]
scalePitches s p =
  map (p `transpose`)
  . concat
  . map shift
  . zip shifts
  $ repeat (scaleIntervals s)
  where
    shifts  = [0,12..]
    shift (s,xs) = map ((+s) . intervalSemitones) xs

scaleSolfege :: Scale -> [String]
scaleSolfege s = concat $ repeat (scaleSolfege_ s)

scaleDegreePitch :: Scale -> Pitch -> ScaleDegree -> Pitch
scaleDegreePitch scale root degree =
  scalePitches scale root !! (degree - 1)

scaleDegreeFromName :: Scale -> String -> Maybe ScaleDegree
scaleDegreeFromName s name =
  case num of
    Just v | v > 0 -> Just v
    _ | n' `elem` scaleSolfege_ s ->
      Just . snd . head . dropWhile ((/= n') . fst) $ zip (scaleSolfege_ s) [1..]
    _ -> Nothing
  where
    num :: Maybe Int
    num = maybeRead name
    n' = map toLower name

scaleDegreeTonicDistance :: Scale -> ScaleDegree -> Semitones
scaleDegreeTonicDistance s deg =
  let a = (deg-1) `mod` (scaleLength s)
      b = (deg-1) `div` (scaleLength s)
  in
   12 * b + intervalSemitones (scaleIntervals s !! a)

scaleDegreeSolfege :: Scale -> ScaleDegree -> String
scaleDegreeSolfege s deg = scaleSolfege s !! (deg - 1)

intervalSemitones :: Interval -> Semitones
intervalSemitones i =
  case i of
    Unison -> 0
    Min2 -> 1
    Maj2 -> 2
    Min3 -> 3
    Maj3 -> 4
    Maj4 -> 5
    Aug4 -> 6
    Maj5 -> 7
    Min6 -> 8
    Maj6 -> 9
    Min7 -> 10
    Maj7 -> 11
    Oct -> 12

notesFromVoice :: Voice -> NoteSeq
notesFromVoice = sequence 0 where
  sequence _ [] = []
  sequence t0 (e:xs) = case e of
    SilenceE dt  -> sequence (t0+dt) xs
    PitchE p dt  -> (p, t0, t0+dt) : sequence (t0+dt) xs
    ChordE ch@(Chord quality root inversion) dt ->
      (map (\p -> (p, t0, t0+dt)) (chordPitches quality root inversion)) ++ sequence (t0+dt) xs
    ParVoicesE vs -> (sortBy (comparing (\(_,t0,_) -> t0)) . concat $ map (sequence t0) vs) ++
                  sequence (t0+dt) xs
                  where dt = voiceDuration [e]

mapTempo :: (Time -> Time) -> NoteSeq -> NoteSeq
mapTempo f = map g where
  g (p,t0,dt) = (p, f t0, f dt)

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
  [(x, _)] -> Just x
  _ -> Nothing
