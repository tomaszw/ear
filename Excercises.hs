{-# LANGUAGE ExistentialQuantification #-}
module Excercises where

import Control.Applicative
import Data.List
import Data.Ord
import Data.Maybe

import Pitch
import Music
import RandomMusic

type CadenceFun = Scale -> Pitch -> Voice

data Query a
   = Query {
       qGenerateContext :: Tonality -> IO Voice
     , qGenerateQuery   :: Tonality -> IO (Voice,Voice,a)
     , qVerify          :: Tonality -> a -> String -> Bool
     , qDescribeAnswer  :: Tonality -> a -> String
     }

simpleContextGen :: CadenceFun -> Tonality -> IO Voice
simpleContextGen cadenceGen tonality = do
  contextOctave <- randomOctave tonality
  let contextRoot = (root tonality) `changeOctave` contextOctave
  return $ cadenceGen (scale tonality) contextRoot
  
randomTonesQuery :: Int -> CadenceFun -> Query [ScaleDegree]
randomTonesQuery len cadenceGen =
  Query {
     qGenerateContext = simpleContextGen cadenceGen
   , qGenerateQuery = genQuery
   , qVerify = verify
   , qDescribeAnswer = describe
   }
  where
    genQuery tonality = do
      (pitches, degs) <- unzip <$> randomNotes len tonality True
      let voice = map (\p -> PitchE p 1) pitches
      return (voice, voice, degs)

    verify tonality correctDegrees answerStr = verifyDegreesStr (scale tonality) correctDegrees answerStr
    describe tonality ans = intercalate " " $ map (scaleDegreeSolfege (scale tonality)) ans

randomToneGroupQuery :: Int -> CadenceFun -> Query [ScaleDegree]
randomToneGroupQuery len cadenceGen =
  Query {
     qGenerateContext = simpleContextGen cadenceGen
   , qGenerateQuery = genQuery
   , qVerify = verify
   , qDescribeAnswer = describe
   }
  where
    genQuery tonality = do
      (pitches, degs) <- unzip . noteSort <$> randomNotes len tonality False
      let voice = [ParVoicesE (map (\p -> [PitchE p 1]) pitches)]
          hint = map (\p -> PitchE p 1) pitches
      return (voice, hint, degs)
      where
        noteSort = sortBy (comparing (\(p,d) -> pitchValue p))
        
    verify tonality correctDegrees answerStr = verifyDegreesStr (scale tonality) correctDegrees answerStr
    describe tonality ans = intercalate " " $ map (scaleDegreeSolfege (scale tonality)) ans

randomProgressionQuery :: Int -> CadenceFun -> Query [ScaleDegree]
randomProgressionQuery len cadenceGen =
  Query {
     qGenerateContext = simpleContextGen cadenceGen
   , qGenerateQuery = genQuery
   , qVerify = verify
   , qDescribeAnswer = describe
   }
  where
    genQuery tonality = do
      (chords, degs) <- unzip <$> randomProgression len tonality
      let voice = map (\chord -> ParVoicesE (map (\p -> [PitchE p 1]) chord)) chords
      return (voice, voice, degs)

    verify tonality correctDegrees answerStr = verifyDegreesStr (scale tonality) correctDegrees answerStr
    describe tonality ans = intercalate " " $ map (scaleDegreeSolfege (scale tonality)) ans

verifyDegreesStr :: Scale -> [ScaleDegree] -> String -> Bool
verifyDegreesStr s correctDegrees answerStr =
      case catMaybes $ map (scaleDegreeFromName s) answer of
        ds | length ds == length answer
           , length ds == length correctDegrees
           , all correct (zip [1..] ds)
              -> True
        _ -> False
      where
        answer = words answerStr
        correct (index, degree) =
          degree == correctDegrees !! (index-1)

