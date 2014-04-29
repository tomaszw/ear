module RandomMusic where

import Control.Applicative
import System.Random

import Music
import Pitch

randomProgression :: Int -> Tonality -> IO [([Pitch],ScaleDegree)]
randomProgression count tonality = do
  (notes,degrees) <- unzip <$> randomNotes count tonality True
  qs <- sequence $ replicate count (randomElement qualities)
  progression <- mapM mkChord $ zip notes qs
  return $ zip progression degrees
  where
    qualities = [ChMaj,ChMin]
    mkChord (n,q) = return $ chordPitches q n 0

randomNotes :: Int -> Tonality -> Bool -> IO [(Pitch,ScaleDegree)]
randomNotes count tonality dupes = gen count
  where
    s = scale tonality
    r = root tonality
    maxO = maxOctave (range tonality)
    minO = minOctave (range tonality)
    octaveSpan = maxO - minO + 1
    degs = degrees tonality

    gen 0 = return []
    gen n = do
      xs <- gen (n-1)
      let loop = do
            x  <- randomNote
            case () of
              _ | dupes -> return x
              _ | not (x `elem` xs) -> return x
              _ -> loop
      x <- loop
      return (x:xs)

    randomNote = do
      let l = scaleLength s
      degree <- case degs of
        [] -> randomInt 1 l
        _  -> randomElement degs
      octave <- randomInt 0 (octaveSpan - 1)
      let d = degree + octave * l
      let pitch   = scaleDegreePitch s (r `changeOctave` minO) d
          solfege = scaleDegreeSolfege s degree
      return (pitch, normaliseDegree s degree)

randomOctave :: Tonality -> IO Int
randomOctave t = randomInt (minOctave $ range t) (maxOctave $ range t)

randomInt :: Int -> Int -> IO Int
randomInt min max = getStdRandom (randomR (min,max))

randomElement :: [a] -> IO a
randomElement xs = do
  v <- randomInt 0 (length xs - 1)
  return $ xs !! v
