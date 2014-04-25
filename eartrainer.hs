module Main where

import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe

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
    deriving (Eq, Show)

type CadenceFun = Scale -> Pitch -> Voice

data Excercise
   = Excercise {
       keyRoot :: Pitch
     , scale :: Scale
     , cadenceGen :: CadenceFun
     , notesTempo :: Int
     , largeRange :: Bool
     , numNotes :: Int
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
  , Option ['q'] [] (ReqArg Questions "NUMBER") "number of questions in exercise"
  , Option ['t'] [] (ReqArg NotesTempo "BPM") "tempo for note playback"
  , Option ['d'] [] (ReqArg Device "DEVICE") "playback midi device"
  , Option ['s'] [] (ReqArg ScaleType "SCALE") "type of scale (maj/min)"
  , Option ['l'] [] (NoArg LargeRange) "large tone range"
  , Option ['p'] [] (ReqArg MidiPgm "NUMBER") "midi program to use for melodies"
  ]

defaultExcercise = Excercise pitchC0 majorScale (cad' cadence_maj_IV_V7_I) 0 False 1 1

parseRequest :: String -> Request
parseRequest "s" = PlayScale
parseRequest "r" = PlayAll
parseRequest "c" = PlayContext
parseRequest "m" = PlayQuestion
parseRequest "h" = Help
parseRequest "t" = PlayTonic
parseRequest "n" = Next
parseRequest "?" = Help
parseRequest str = Other str

playMusic :: Player -> Music -> IO ()
playMusic p m = playVoice p (musicTempo m) (musicPgm m) (musicVoice m)

playQuestion :: Player -> Question -> IO ()
playQuestion p q = playMusic p (questionContext q) >> playMusic p (question q)

askQuestion :: String -> (Request -> IO a) -> ([String] -> Bool) -> IO Int
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
               ws -> case testAnswer ws of
                 True -> putStrLn "GOOD!" >> return attempts
                 _    -> putStrLn "NO!"   >> ask (attempts+1)
           req -> handleRequest req >> ask attempts

handleRequest :: Player -> Question -> Request -> IO ()
handleRequest p q req = h req where
  tonicPitch = questionScaleRoot q `changeOctave` 4
  h PlayScale    = playVoice p (BPM 120) 0 (map (\p -> PitchE p 1) pitches) where
                   pitches = take (scaleLength scale + 1) $ scalePitches scale tonicPitch
                   scale = questionScale q
  h PlayContext  = playMusic p (questionContext q)
  h PlayQuestion   = playMusic p (question q)
  h PlayAll = playQuestion p q
  h PlayTonic    = playVoice p (BPM 120) 0 [PitchE tonicPitch 4]
  h Help         = putStrLn "'r' - repeat/play full question 'c' - play cadence 'm' - play melody 's' - play scale 'h' - help"
  h _            = return ()
  
randomNotes :: Int -> Pitch -> Scale -> Int -> Int -> IO [(Pitch,ScaleDegree)]
randomNotes count rootPitch scale minOctave maxOctave =
  sequence $ replicate count randomNote
  where
    octaveSpan = maxOctave - minOctave + 1
    randomNote = do
      degree <- randomInt 1 (octaveSpan * 12)
      let pitch   = scaleDegreePitch scale (rootPitch `changeOctave` minOctave) degree
          solfege = scaleDegreeSolfege scale degree
      return (pitch, normaliseDegree scale degree)
  
excercise :: Player -> Excercise -> Int -> IO Stats
excercise p ex q | q > numQuestions ex = return $ Stats 0 0
excercise p ex@(Excercise root scale contextGen notesTempo
                largeRange numNotes numQuestions pgm) currentQ = do
    contextOctave <- if largeRange then randomInt 2 5 else randomInt 3 4
    let contextRoot = root `changeOctave` contextOctave
    
    octave <- if largeRange then randomInt 1 4 else randomInt 2 3
    (pitches,chosenDegrees) <- unzip <$> randomNotes numNotes root scale octave (octave+1)
    let chosenSolfege = map (scaleDegreeSolfege scale) chosenDegrees
    let melodyRoot = root `changeOctave` octave
        melody  = map (\p -> PitchE p 1) pitches
        quest   = Question {
            questionContext = Music (BPM 120) 0 (contextGen scale contextRoot)
          , questionScale = scale
          , questionScaleRoot = root
          , question = Music (BPM notesTempo) pgm melody
          }

        testAnswer answer =
          case catMaybes $ map (scaleDegreeFromName scale) answer of
            degrees | length degrees == length answer
                    , length degrees == length chosenDegrees
                    , all correct (zip [1..] degrees)
                    -> True
            _ -> False
          where
          correct (index, degree) =
            degree == chosenDegrees !! (index-1)
        
    guessedIn <- askQuestion prompt (handleRequest p quest) testAnswer
    when (guessedIn == 0) $ do
      putStrLn "Correct answer was:"
      mapM_ (\l -> putStrLn ("  " ++ l)) $ map (pitchDesc contextRoot) $ zip pitches chosenSolfege
    putStrLn "Press ENTER to continue.."
    let loopReq =
          do putStr ">> "
             hFlush stdout
             req <- parseRequest `fmap` getLine
             case req of
               Other "" -> return ()
               _ -> handleRequest p quest req >> loopReq
    loopReq
    Stats correct wrong <- excercise p ex (currentQ+1)
    return $ 
      if guessedIn == 1
         then Stats (correct+1) wrong
         else Stats correct (wrong+1)
    where
      prompt = show currentQ ++ ". [" ++ solfegeStr ++ "] >> "
      solfegeStr = intercalate " " $ take (scaleLength scale) (scaleSolfege scale)
      pitchDesc contextRoot (pitch, solfege) =
          solfege ++ "         -> " ++ show pitch ++ " in key " ++ (show contextRoot)

scaleCadence :: Scale -> Pitch -> Voice
scaleCadence s root =
  map (\p -> PitchE p 0.5) pitches ++ [SilenceE 2] where
    pitches = take (scaleLength s + 1) $ scalePitches s root

cad' c s r = c r

scaleOfStr "maj" = (majorScale, cad' cadence_maj_IV_V7_I)
scaleOfStr "chmaj" = (majorChScale, cad' cadence_maj_IV_V7_I)
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
      getKeyRoot (KeyRoot str) x = case pitchFromName str of
        Just p -> p
        _ -> error $ "bad pitch name " ++ str
      getKeyRoot _ x = x
      
  Stats correct wrong <- withPlayer device $ \p ->
    excercise p (Excercise keyRoot scaleType cadence notesTempo largeRange notes questions pgm) 1
  putStrLn $ "CORRECT " ++ show correct ++ " out of " ++ show (correct+wrong)
  
  where
    header = "usage: "
  
