module Scales where

import Pitch
import Music

ch r dur q i = ChordE (Chord q (r `transpose` i)) dur

cadence_maj_IV_V7_I :: Pitch -> Voice
cadence_maj_IV_V7_I r =
  [
    ch r 1.5 ChMajor Maj4
  , ch r 1.2 ChMajorD7 Maj5
  , ch r 0.75 ChMajorD7 Maj5
  , ch r 3.0 ChMajor Unison
  ]

cadence_min_IV_V7_I :: Pitch -> Voice
cadence_min_IV_V7_I r =
  [
    ch r 1.5 ChMinor Maj4
  , ch r 1.2 ChMinorD7 Maj5
  , ch r 0.75 ChMinorD7 Maj5
  , ch r 3.0 ChMinor Unison
  ]

majorScale = Scale "major" [Unison, Maj2, Maj3, Maj4, Maj5, Maj6, Maj7] majorScaleSolfege
majorScaleSolfege = ["do", "re", "mi", "fa", "so", "la", "ti"]

minorScale = Scale "minor" [Unison, Maj2, Min3, Maj4, Maj5, Min6, Min7] minorScaleSolfege
minorScaleSolfege = ["do", "re", "me", "fa", "so", "le", "te"]

bluesScale = Scale "blues" [Unison, Min3, Maj4, Aug4, Maj5, Min7] bluesScaleSolfege
bluesScaleSolfege = ["do", "me", "fa", "fi", "so", "te"]

majorChScale = Scale "chmajor" [Unison, Min2, Maj2, Min3, Maj3, Maj4, Aug4, Maj5, Min6, Maj6, Min7, Maj7] majorChScaleSolfege
majorChScaleSolfege = ["do", "di", "re", "ri", "mi", "fa", "fi", "so", "si", "la", "li", "ti"]

majorChScaleM = Scale "chmajor" [Min2, Min3, Aug4, Min6, Min7] majorChScaleMSolfege
majorChScaleMSolfege = ["di", "ri", "fi", "si", "li"]

