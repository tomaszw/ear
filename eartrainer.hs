module Main where

import Control.Monad

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
  | Questions String
  | KeyRoot String
    deriving (Eq, Show)

data Excercise
   = Excercise {
       keyRoot :: Pitch
     , scale :: Scale
     , numNotes :: Int
     , numQuestions :: Int }

data Request
   = PlayScale
   | PlayQuestion
   | PlayCadence
   | PlayMelody
   | PlayTonic
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
  , Option ['q'] [] (ReqArg Questions "NUMBER") "number of questions in excercise"
  , Option ['d'] [] (ReqArg Device "DEVICE") "playback midi device"
  ]

defaultExcercise = Excercise pitchC0 majorScale 1 1

parseRequest :: String -> Request
parseRequest "s" = PlayScale
parseRequest "r" = PlayQuestion
parseRequest "c" = PlayCadence
parseRequest "m" = PlayMelody
parseRequest "h" = Help
parseRequest "t" = PlayTonic
parseRequest "?" = Help
parseRequest str = Other str

excercise :: Player -> Excercise -> Int -> IO Stats
excercise p (Excercise _ _ _ numQuestions) q | q > numQuestions = return $ Stats 0 0
excercise p ex@(Excercise root scale numNotes numQuestions) currentQ = do
    let tempo = BPM 120
    cadenceOctave <- randomInt 3 4
    let cadenceRoot = root `changeOctave` cadenceOctave
    
    octave  <- randomInt 3 4
    pitches <- genPitches octave numNotes
    let melodyRoot = root `changeOctave` octave
        melody  = map (\(p,solfege) -> PitchE p 2) pitches
        cadence = cadence_maj_IV_V7_I cadenceRoot 
        q = cadence ++ melody
    let handleRequest PlayScale =
          let pitches = take (scaleLength scale + 1) $ scalePitches scale melodyRoot in
          playVoice p tempo (map (\p -> PitchE p 1) pitches)
        handleRequest PlayCadence = playVoice p tempo cadence
        handleRequest PlayMelody = playVoice p tempo melody
        handleRequest PlayQuestion = playVoice p tempo q
        handleRequest PlayTonic = playVoice p tempo [PitchE melodyRoot 2]
        handleRequest Help = putStrLn "'r' - repeat question 'c' - play cadence 'm' - play melody 's' - play scale 'h' - help"
        handleRequest _ = return ()
    answer <- question handleRequest q
    let correctAnswer = map (\(_,solfege) -> solfege) pitches
        ok = answer == correctAnswer
    if ok
       then putStrLn "GOOD!"
       else putStrLn "BAD!"
    mapM_ (\l -> putStrLn ("  " ++ l)) $ map (pitchDesc cadenceRoot) pitches
    putStrLn "Press ENTER to continue.."
    let loopReq =
          do req <- parseRequest `fmap` getLine
             case req of
               Other "" -> return ()
               _ -> handleRequest req >> loopReq
    loopReq
    Stats correct wrong <- excercise p ex (currentQ+1)
    return $ 
      if ok then Stats (correct+1) wrong
            else Stats correct (wrong+1)
    where
      question handleRequest melody = do
        putStrLn $ "Question " ++ show currentQ
        hFlush stdout
        playVoice p (BPM 120) melody
        let ask =
              do putStr ">> "
                 hFlush stdout
                 r <- return . parseRequest =<< getLine
                 case r of
                   Other str -> case words str of
                     [] -> question handleRequest melody
                     ws -> return ws
                   req       -> handleRequest req >> ask
        ask

      genPitch octave = do
        degree <- randomInt 1 (scaleLength scale)
        let pitch   = scaleDegreePitch scale root degree `changeOctave` octave
            solfege = scaleDegreeSolfege scale degree
        return (pitch, solfege)
        
      genPitches octave n = sequence . replicate n $ genPitch octave
     
      pitchDesc cadenceRoot (pitch, solfege) =
          solfege ++ "         -> " ++ show pitch ++ " in key " ++ (show cadenceRoot)

main = do
  args_ <- getArgs
  (flags, args) <- case getOpt Permute options args_ of
    (o,n,[]) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  let device = foldr getDevice "129:0" flags
      notes = foldr getNotes 1 flags
      questions = foldr getQuestions 1 flags
      keyRoot = foldr getKeyRoot pitchC0 flags
      randomKey = RandomKey `elem` flags
      getDevice (Device str) _ = str
      getDevice _ str = str
      getNotes (Notes str) _ = read str
      getNotes _ x = x
      getQuestions (Questions str) _ = read str
      getQuestions _ x = x
      getKeyRoot (KeyRoot str) x = case pitchFromName str of
        Just p -> p
        _ -> error $ "bad pitch name " ++ str
      getKeyRoot _ x = x
      
  Stats correct wrong <- withPlayer device $ \p ->
    excercise p (Excercise keyRoot majorScale notes questions) 1
  putStrLn $ "CORRECT " ++ show correct ++ " out of " ++ show (correct+wrong)
  
  where
    header = "usage: "
  
