module Main where

import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Ord

import System.Environment
import System.Console.GetOpt
import System.IO

import Pitch
import Music
import Scales
import Player

data Flag
  = RandomKey
  | Device String
  | Notes String 
  | NotesTempo String
  | Questions String
  | KeyRoot String
  | ScaleType String
  | LargeRange
  | MidiPgm String
  | GuessChord
  | Degrees String
    deriving (Eq, Show)

type CadenceFun = Scale -> Pitch -> Voice

data ToneRange
   = ToneRange { minOctave :: Int, maxOctave :: Int } deriving (Eq,Show)

smallToneRange  = ToneRange 3 4
mediumToneRange = ToneRange 3 5
largeToneRange  = ToneRange 2 5

data Tonality
   = Tonality {
     scale :: Scale
   , degrees :: [ScaleDegree]
   , root :: Pitch
   , range :: ToneRange
   } deriving (Eq, Show)

defaultTonality = Tonality majorScale [] pitchC0 mediumToneRange

data Query a
   = Query {
       qGenerateContext :: Tonality -> IO Voice
     , qGenerateQuery   :: Tonality -> IO (Voice,a)
     , qVerify          :: Tonality -> a -> String -> Bool
     , qDescribeAnswer  :: Tonality -> a -> String
     }

data Excercise
   = Excercise {
       tonality :: Tonality
     , notesTempo :: Int
     , numQuestions :: Int
     , midiPgm :: Int
     }

data Music
   = Music {
       musicTempo :: BPM
     , musicPgm :: Int
     , musicVoice :: Voice
     }

data Question
   = Question {
       questionContext :: Music
     , questionScale :: Scale
     , questionScaleRoot :: Pitch
     , question :: Music
     }

data Request
   = PlayScale
   | PlayAll
   | PlayContext
   | PlayQuestion
   | PlayTonic
   | PlayTones [String]
   | Next
   | Help
   | Other String

data Stats
   = Stats {
       correct :: Int
     , wrong :: Int }

options :: [OptDescr Flag]
options =
  [ Option ['r'] [] (NoArg RandomKey) "randomize key"
  , Option ['k'] [] (ReqArg KeyRoot "PITCH") "specify key, such as C,C#,D,D#,E,F,F#,G,G#,B"
  , Option ['n'] [] (ReqArg Notes "NUMBER") "number of notes to guess at once"
  , Option ['c'] [] (NoArg GuessChord) "group notes to guess in a chord"
  , Option ['q'] [] (ReqArg Questions "NUMBER") "number of questions in exercise"
  , Option ['t'] [] (ReqArg NotesTempo "BPM") "tempo for note playback"
  , Option ['d'] [] (ReqArg Device "DEVICE") "playback midi device"
  , Option ['s'] [] (ReqArg ScaleType "SCALE") "type of scale (maj/min)"
  , Option ['l'] [] (NoArg LargeRange) "large tone range"
  , Option ['p'] [] (ReqArg MidiPgm "NUMBER") "midi program to use for melodies"
  , Option ['x'] [] (ReqArg Degrees "DEGREES") "pick scale degrees to use"
  ]

parseRequest :: String -> Request
parseRequest "s" = PlayScale
parseRequest "r" = PlayAll
parseRequest "c" = PlayContext
parseRequest "m" = PlayQuestion
parseRequest "h" = Help
parseRequest "t" = PlayTonic
parseRequest "n" = Next
parseRequest "?" = Help
parseRequest str | ("p" : tones) <- words str = PlayTones tones
parseRequest str = Other str

playMusic :: Player -> Music -> IO ()
playMusic p m = playVoice p (musicTempo m) (musicPgm m) (musicVoice m)

playQuestion :: Player -> Question -> IO ()
playQuestion p q = playMusic p (questionContext q) >> playMusic p (question q)

askQuestion :: String -> (Request -> IO a) -> (String -> Bool) -> IO Int
askQuestion prompt handleRequest testAnswer =
  handleRequest PlayAll >> ask 1
  where
    ask attempts =
      do putStr prompt
         hFlush stdout
         r <- return . parseRequest =<< getLine
         case r of
           Next -> return 0
           Other str ->
             case words str of
               [] -> handleRequest PlayAll >> ask attempts
               _  -> case testAnswer str of
                 True -> putStrLn "GOOD!" >> return attempts
                 _    -> putStrLn "NO!"   >> ask (attempts+1)
           req -> handleRequest req >> ask attempts

handleRequest :: Player -> Question -> Request -> IO ()
handleRequest p q req = h req where
  tonicPitch = questionScaleRoot q `changeOctave` 4
  scale = questionScale q
  h PlayScale    = playVoice p (BPM 120) 0 (map (\p -> PitchE p 1) pitches) where
                   pitches = take (scaleLength scale + 1) $ scalePitches scale tonicPitch
  h PlayContext  = playMusic p (questionContext q)
  h PlayQuestion   = playMusic p (question q)
  h PlayAll = playQuestion p q
  h PlayTonic    = playVoice p (BPM 120) 0 [PitchE tonicPitch 4]
  h (PlayTones tones) = playMusic p ((question q) { musicVoice = map (\p -> PitchE p 1) pitches }) where
                        pitches = map (scaleDegreePitch scale tonicPitch) degrees
                        degrees = catMaybes $ map (scaleDegreeFromName scale) tones
  h Help         = putStrLn "'r' - repeat/play full question 'c' - play cadence 'm' - play melody 's' - play scale 'h' - help"
  h _            = return ()

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy x xs =
  let (p,q) = break (== x) xs
      xs'   = splitBy x $ dropWhile (== x) q in
  if null p then xs' else p : xs'

parseDegrees :: String -> Scale -> [ScaleDegree]
parseDegrees str scale = catMaybes . map (scaleDegreeFromName scale) $ splitBy ',' str

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
      (pitches, degs) <- unzip <$> randomNotes len tonality False
      let voice = map (\p -> PitchE p 1) pitches
      return (voice, degs)

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
      let voice = [VoicesE (map (\p -> [PitchE p 1]) pitches)]
      return (voice, degs)
      where
        noteSort = sortBy (comparing (\(p,d) -> pitchValue p))
        
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

excercise :: Player -> Excercise -> Query a -> Int -> IO Stats
excercise p ex _ q | q > numQuestions ex = return $ Stats 0 0
excercise p ex@(Excercise tonality notesTempo numQuestions pgm) query currentQ = do
    context <- qGenerateContext query tonality
    (voice,qdata) <- qGenerateQuery query tonality

    let
        quest   = Question {
            questionContext = Music (BPM 120) 0 context
          , questionScale = s
          , questionScaleRoot = r
          , question = Music (BPM notesTempo) pgm voice
          }

    guessedIn <- askQuestion prompt (handleRequest p quest) (qVerify query tonality qdata)
    when (guessedIn == 0) $ do
      putStrLn "Correct answer was:"
      putStrLn $ "  " ++ qDescribeAnswer query tonality qdata
    putStrLn "Press ENTER to continue.."
    let loopReq =
          do putStr ">> "
             hFlush stdout
             req <- parseRequest `fmap` getLine
             case req of
               Other "" -> return ()
               _ -> handleRequest p quest req >> loopReq
    loopReq
    Stats correct wrong <- excercise p ex query (currentQ+1)
    return $ 
      if guessedIn == 1
         then Stats (correct+1) wrong
         else Stats correct (wrong+1)
    where
      s = scale tonality
      r = root tonality
      prompt = show currentQ ++ ". [" ++ solfegeStr ++ "] >> "
      solfegeStr = intercalate " " $ take (scaleLength s) (scaleSolfege s)
    
scaleCadence :: Scale -> Pitch -> Voice
scaleCadence s root =
  map (\p -> PitchE p 0.5) pitches ++ [SilenceE 2] where
    pitches = take (scaleLength s + 1) $ scalePitches s root

cad' c s r = c r

scaleOfStr "maj" = (majorScale, cad' cadence_maj_IV_V7_I)
scaleOfStr "chmaj" = (majorChScale, cad' cadence_maj_IV_V7_I)
scaleOfStr "chmaj-" = (majorChScaleM, cad' cadence_maj_IV_V7_I)
scaleOfStr "min" = (minorScale, cad' cadence_min_IV_V7_I)
scaleOfStr "blu" = (bluesScale, scaleCadence)

scaleOfStr _ = error "unknown scale"

main = do
  args_ <- getArgs
  (flags, args) <- case getOpt Permute options args_ of
    (o,n,[]) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  let device = foldr getDevice "128:0" flags
      notes = foldr getNotes 1 flags
      pgm = foldr getPgm 0 flags
      notesTempo = foldr getNotesTempo 30 flags
      questions = foldr getQuestions 10 flags
      keyRoot = foldr getKeyRoot pitchC0 flags
      degreesstr = foldr getDegrees [] flags
      asCh = GuessChord `elem` flags
      randomKey = RandomKey `elem` flags
      largeRange = LargeRange `elem` flags
      (scaleType,cadence) = foldr getST (majorScale,cad' cadence_maj_IV_V7_I) flags
      getDevice (Device str) _ = str
      getDevice _ str = str
      getST (ScaleType str) _ = scaleOfStr str
      getST _ x = x
      getNotes (Notes str) _ = read str
      getNotes _ x = x
      getPgm (MidiPgm str) _ = read str
      getPgm _ x = x
      getNotesTempo (NotesTempo str) _ = read str
      getNotesTempo _ x = x
      getQuestions (Questions str) _ = read str
      getQuestions _ x = x
      getDegrees (Degrees str) _ = str
      getDegrees _ x = x
      getKeyRoot (KeyRoot str) x = case pitchFromName str of
        Just p -> p
        _ -> error $ "bad pitch name " ++ str
      getKeyRoot _ x = x

  let degs = parseDegrees degreesstr scaleType
      tonality = Tonality scaleType degs keyRoot mediumToneRange
      q | not asCh = randomTonesQuery notes cadence
        | otherwise = randomToneGroupQuery notes cadence
  Stats correct wrong <- withPlayer device $ \p ->
    excercise p (Excercise tonality notesTempo questions pgm) q 1
  putStrLn $ "CORRECT " ++ show correct ++ " out of " ++ show (correct+wrong)
  
  where
    header = "usage: "
  
