{-# LANGUAGE ExistentialQuantification #-}
module Music where

import Data.Word
import Data.Maybe
import System.Random

import Pitch

type Duration = Double
type Time = Double
newtype BPM = BPM Int deriving (Eq,Show)

data Interval =
    Unison
  | Min2
  | Maj2
  | Min3
  | Maj3
  | Maj4
  | Aug4
  | Maj5
  | Min6
  | Maj6
  | Min7
  | Maj7
  | Oct
    deriving (Eq,Show)

data Scale =
     Scale {
       scaleName :: String
     , scaleIntervals :: [Interval]
     , scaleSolfege_ :: Solfege
     } deriving (Eq, Show)

type Solfege = [String]

data Chord =
     Chord {
       chordRoot :: Pitch
     , chordExtensions :: [Int]
     }
   | ChordI {
       chordRoot :: Pitch
     , chordIExtensions :: [Interval]
   }
   deriving (Eq, Show)

type Voice = [VoiceElement]
data VoiceElement =
    PitchE Pitch Duration
  | ChordE Chord Duration
  | SilenceE Duration

type SequencedVoice = [(Pitch,Duration,Duration)]

chordMaj p = ChordI p [Maj3, Maj5]
chordMin p = ChordI p [Min3, Maj5]
chordMajD7 p = ChordI p [Maj3, Maj5, Min7]
chordMinD7 p = ChordI p [Min3, Maj5, Min7]
chordMaj7 p = ChordI p [Maj3, Maj5, Maj7]

chordOnScale :: Scale -> Pitch -> [Int] -> Chord
chordOnScale scale scaleRoot degrees =
  let dists = map (scaleDegreeTonicDistance scale) degrees
      dist0 = head dists
  in
  Chord (scaleRoot `transpose` head dists) (map (\d -> d - dist0) $ tail dists)

cadence_minmaj_IV_V7_I :: Scale -> Pitch -> Voice
cadence_minmaj_IV_V7_I scale scaleRoot =
  let ch dur degs = ChordE (chordOnScale scale scaleRoot degs) dur in
  [ ch 1.5 [1, 4, 6]
  , ch 1.5 [2, 4, 5, 7]
  , ch 3.0 [1, 3, 5] ]

cadence_chmaj_IV_V7_I :: Scale -> Pitch -> Voice
cadence_chmaj_IV_V7_I scale scaleRoot =
  let ch dur degs = ChordE (chordOnScale scale scaleRoot degs) dur in
  [ ch 1.5 [1, 6, 10]
  , ch 1.5 [3, 6, 8, 12]
  , ch 3.0 [1, 5, 8] ]

cadence_maj_IV_V7_I :: Pitch -> Voice
cadence_maj_IV_V7_I r =
  [ ChordE (chordMaj $ r `shiftI` Maj4) 1.5
  , ChordE (chordMajD7 $ r `shiftI` Maj5) 1.5
  , ChordE (chordMaj r) 2
  ]

majorScale = Scale "major" [Unison, Maj2, Maj3, Maj4, Maj5, Maj6, Maj7] majorScaleSolfege
majorScaleSolfege = ["do", "re", "mi", "fa", "so", "la", "ti"]

minorScale = Scale "minor" [Unison, Maj2, Min3, Maj4, Maj5, Min6, Min7] minorScaleSolfege
minorScaleSolfege = ["do", "re", "me", "fa", "so", "le", "te"]

majorChScale = Scale "chmajor" [Unison, Min2, Maj2, Min3, Maj3, Maj4, Aug4, Maj5, Min6, Maj6, Min7, Maj7] majorChScaleSolfege
majorChScaleSolfege = ["do", "di", "re", "ri", "mi", "fa", "fi", "so", "si", "la", "li", "ti"]

shiftI :: Pitch -> Interval -> Pitch
shiftI p i = p `transpose` intervalSemitones i

scaleLength :: Scale -> Int
scaleLength = length . scaleIntervals

scalePitches :: Scale -> Pitch -> [Pitch]
scalePitches s p =
  map (p `transpose`)
  . concat
  . map shift
  . zip shifts
  $ repeat (map fromIntegral $ map intervalSemitones (scaleIntervals s))
  where
    shifts  = [0,12..]
    shift (s,xs) = map (s+) xs

scaleSolfege :: Scale -> [String]
scaleSolfege s = concat $ repeat (scaleSolfege_ s)

scaleDegreePitch :: Scale -> Pitch -> Int -> Pitch
scaleDegreePitch scale root degree =
  scalePitches scale root !! (degree - 1)

scaleDegreeTonicDistance :: Scale -> Int -> Int
scaleDegreeTonicDistance s deg =
  let a = (deg-1) `mod` (scaleLength s)
      b = (deg-1) `div` (scaleLength s)
  in
   12 * b + intervalSemitones (scaleIntervals s !! a)

scaleDegreeSolfege :: Scale -> Int -> String
scaleDegreeSolfege s deg = scaleSolfege s !! (deg - 1)

intervalSemitones :: Interval -> Int
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

notesFromVoice :: Voice -> SequencedVoice
notesFromVoice = sequence 0 where
  sequence _ [] = []
  sequence t0 (e:xs) = case e of
    SilenceE dt  -> sequence (t0+dt) xs
    PitchE p dt  -> (p, t0, t0+dt) : sequence (t0+dt) xs
    ChordE ch dt -> chordNotes dt ch ++ sequence (t0+dt) xs
    where
      chordNotes dt (Chord root exts) =
        (root, t0, t0+dt) : map extNote exts where
          extNote i = (root `transpose` i, t0, t0+dt)
      chordNotes dt (ChordI root ints) = chordNotes dt (Chord root (map intervalSemitones ints))

mapTempo :: (Time -> Time) -> SequencedVoice -> SequencedVoice
mapTempo f = map g where
  g (p,t0,dt) = (p, f t0, f dt)

randomInt :: Int -> Int -> IO Int
randomInt min max = getStdRandom (randomR (min,max))

randomElement :: [a] -> IO a
randomElement xs = do
  v <- randomInt 0 (length xs - 1)
  return $ xs !! v