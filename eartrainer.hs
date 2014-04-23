module Main where

import Control.Monad
import Data.List

import System.Environment
import System.Console.GetOpt
import System.IO

import Pitch
import Music
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
     , numQuestions :: Int }

data Request
   = PlayScale
   | PlayQuestion
   | PlayCadence
   | PlayMelody
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
  , Option ['k'] [] (ReqArg KeyRoot "PITCH") "key root, such as C,C#,D,D#,E,F,F#,G,G#,B"
  , Option ['n'] [] (ReqArg Notes "NUMBER") "number of notes to guess at once"
  , Option ['q'] [] (ReqArg Questions "NUMBER") "number of questions in exercise"
  , Option ['t'] [] (ReqArg NotesTempo "BPM") "tempo for note playback"
  , Option ['d'] [] (ReqArg Device "DEVICE") "playback midi device"
  , Option ['s'] [] (ReqArg ScaleType "SCALE") "type of scale (maj/min)"
  , Option ['l'] [] (NoArg LargeRange) "large tone range"
  ]

defaultExcercise = Excercise pitchC0 majorScale cadence_minmaj_IV_V7_I 60 False 1 1

parseRequest :: String -> Request
parseRequest "s" = PlayScale
parseRequest "r" = PlayQuestion
parseRequest "c" = PlayCadence
parseRequest "m" = PlayMelody
parseRequest "h" = Help
parseRequest "t" = PlayTonic
parseRequest "n" = Next
parseRequest "?" = Help
parseRequest str = Other str

question :: String -> (Request -> IO a) -> ([String] -> Bool) -> IO Int
question prompt handleRequest testAnswer =
  handleRequest PlayQuestion >> ask 1
  where
    ask attempts =
      do putStr prompt
         hFlush stdout
         r <- return . parseRequest =<< getLine
         case r of
           Next -> return 0
           Other str ->
             case words str of
               [] -> handleRequest PlayQuestion >> ask attempts
               ws -> case testAnswer ws of
                 True -> putStrLn "GOOD!" >> return attempts
                 _    -> putStrLn "NO!"   >> ask (attempts+1)
           req -> handleRequest req >> ask attempts

excercise :: Player -> Excercise -> Int -> IO Stats
excercise p ex q | q > numQuestions ex = return $ Stats 0 0
excercise p ex@(Excercise root scale@(Scale _ solf) cadenceGen notesTempo
                largeRange numNotes numQuestions) currentQ = do
    let tempo = BPM 120
    cadenceOctave <- if largeRange then randomInt 2 5 else randomInt 3 4
    let cadenceRoot = root `changeOctave` cadenceOctave
    
    octave  <- if largeRange then randomInt 1 5 else randomInt 2 4
    pitches <- genPitches octave numNotes
    let melodyRoot = root `changeOctave` octave
        melody  = map (\(p,solfege) -> PitchE p 2) pitches
        cadence = cadenceGen scale cadenceRoot 
    let handleRequest PlayScale =
          let pitches = take (scaleLength scale + 1) $ scalePitches scale melodyRoot in
          playVoice p tempo (map (\p -> PitchE p 1) pitches)
        handleRequest PlayCadence = playVoice p tempo cadence
        handleRequest PlayMelody = playVoice p (BPM notesTempo) melody
        handleRequest PlayQuestion = playVoice p tempo cadence >> playVoice p (BPM notesTempo) melody
        handleRequest PlayTonic = playVoice p tempo [PitchE melodyRoot 2]
        handleRequest Help = putStrLn "'r' - repeat question 'c' - play cadence 'm' - play melody 's' - play scale 'h' - help"
        handleRequest _ = return ()
    let testAnswer s =
          let correctAnswer = map (\(_,solfege) -> solfege) pitches in
          s == correctAnswer
    let prompt = show currentQ ++ ". [" ++ solfegeStr ++ "] >> "
        solfegeStr = intercalate " " solf
        
    guessedIn <- question prompt handleRequest testAnswer
    when (guessedIn == 0) $ do
      putStrLn "Correct answer was:"
      mapM_ (\l -> putStrLn ("  " ++ l)) $ map (pitchDesc cadenceRoot) pitches
    putStrLn "Press ENTER to continue.."
    let loopReq =
          do putStr ">> "
             hFlush stdout
             req <- parseRequest `fmap` getLine
             case req of
               Other "" -> return ()
               _ -> handleRequest req >> loopReq
    loopReq
    Stats correct wrong <- excercise p ex (currentQ+1)
    return $ 
      if guessedIn == 1
         then Stats (correct+1) wrong
         else Stats correct (wrong+1)
    where
      genPitch octave_ = do
        octave  <- if largeRange then randomInt 1 6 else randomInt octave_ (octave_+1)
        degree <- randomInt 1 (scaleLength scale + 1)
        let pitch   = scaleDegreePitch scale root degree `changeOctave` octave
            solfege = scaleDegreeSolfege scale degree
        return (pitch, solfege)
        
      genPitches octave n = sequence . replicate n $ genPitch octave
     
      pitchDesc cadenceRoot (pitch, solfege) =
          solfege ++ "         -> " ++ show pitch ++ " in key " ++ (show cadenceRoot)

scaleOfStr "maj" = (majorScale, cadence_minmaj_IV_V7_I)
scaleOfStr "chmaj" = (majorChScale, cadence_chmaj_IV_V7_I)
scaleOfStr "min" = (minorScale, cadence_minmaj_IV_V7_I)
scaleOfStr _ = error "unknown scale"

main = do
  args_ <- getArgs
  (flags, args) <- case getOpt Permute options args_ of
    (o,n,[]) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  let device = foldr getDevice "129:0" flags
      notes = foldr getNotes 1 flags
      notesTempo = foldr getNotesTempo 120 flags
      questions = foldr getQuestions 10 flags
      keyRoot = foldr getKeyRoot pitchC0 flags
      randomKey = RandomKey `elem` flags
      largeRange = LargeRange `elem` flags
      (scaleType,cadence) = foldr getST (majorScale,cadence_minmaj_IV_V7_I) flags
      getDevice (Device str) _ = str
      getDevice _ str = str
      getST (ScaleType str) _ = scaleOfStr str
      getST _ x = x
      getNotes (Notes str) _ = read str
      getNotes _ x = x
      getNotesTempo (NotesTempo str) _ = read str
      getNotesTempo _ x = x
      getQuestions (Questions str) _ = read str
      getQuestions _ x = x
      getKeyRoot (KeyRoot str) x = case pitchFromName str of
        Just p -> p
        _ -> error $ "bad pitch name " ++ str
      getKeyRoot _ x = x
      
  Stats correct wrong <- withPlayer device $ \p ->
    excercise p (Excercise keyRoot scaleType cadence notesTempo largeRange notes questions) 1
  putStrLn $ "CORRECT " ++ show correct ++ " out of " ++ show (correct+wrong)
  
  where
    header = "usage: "
  
