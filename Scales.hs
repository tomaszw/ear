module Scales where

import Pitch
import Music

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

