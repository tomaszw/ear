module Main where

import Control.Monad
import Control.Applicative
import Data.List hiding (transpose)
import Data.Maybe
import Data.Ord

import System.Environment
import System.Console.GetOpt
import System.IO
import Control.Concurrent

import Pitch
import Music
import RandomMusic
import Scales
import Excercises
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
  | Degrees String
  | ExcerciseType String
  | Range String
  | ContextEvery String
    deriving (Eq, Show)

smallToneRange  = ToneRange 3 4
mediumToneRange = ToneRange 3 5
largeToneRange  = ToneRange 2 5

data Excercise
   = Excercise {
       tonality :: Tonality
     , notesTempo :: Int
     , numQuestions :: Int
     , midiPgm :: Int
     , contextFrequency :: Int
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
     deriving Show
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
  , Option ['x'] [] (ReqArg Degrees "DEGREES") "pick scale degrees to use"
  , Option ['e'] [] (ReqArg ExcerciseType "STRING") "type of excercise melody/group/progression"
  , Option ['c'] [] (ReqArg ContextEvery "NUMBER") "play context cadence every NUMBER questions"
  , Option [] ["range"] (ReqArg Range "OCTAVE-OCTAVE") "tone range for excercise, in octaves"
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

askQuestion :: String -> Request -> (Request -> IO a) -> (String -> Bool) -> IO Int
askQuestion prompt playR handleRequest testAnswer =
  doreq >> ask 1
  where
    ask attempts =
      do putStr prompt
         hFlush stdout
         r <- return . parseRequest =<< getLine
         case r of
           Next -> return 0
           Other str ->
             case words str of
               [] -> doreq >> ask attempts
               _  -> case testAnswer str of
                 True -> putStrLn "GOOD!" >> return attempts
                 _    -> putStrLn "NO!"   >> ask (attempts+1)
           req -> handleRequest req >> ask attempts
    doreq = handleRequest playR
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

excercise :: Player -> Excercise -> Bool -> Query a -> Int -> IO Stats
excercise p ex _ _ q | q > numQuestions ex = return $ Stats 0 0
excercise p ex@(Excercise tonality' notesTempo numQuestions pgm contextFreq) randomKey query currentQ = do
    tonality <- updateTonality
    context <- qGenerateContext query tonality
    (voice,qdata) <- qGenerateQuery query tonality

    let
        quest   = Question {
            questionContext = Music (BPM 120) 0 context
          , questionScale = s tonality
          , questionScaleRoot = r tonality
          , question = Music (BPM notesTempo) pgm voice
          }

        playReq = case contextFreq of
          0 -> PlayAll
          n | ((currentQ-1) `mod` n) == 0 -> PlayAll
          _ -> PlayQuestion
    guessedIn <- askQuestion (prompt tonality) playReq (\x -> {-void . forkIO . void $-} handleRequest p quest x) (qVerify query tonality qdata)
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
    Stats correct wrong <- excercise p ex randomKey query (currentQ+1)
    return $ 
      if guessedIn == 1
         then Stats (correct+1) wrong
         else Stats correct (wrong+1)
    where
      s t = scale t
      r t = root t
      prompt t = show currentQ ++ ". [" ++ solfegeStr t ++ "] >> "
      solfegeStr t = intercalate " " $ take (scaleLength $ s t) (scaleSolfege $ s t)
      updateTonality = do
        if not randomKey
           then return tonality'
           else do
             randomKeyRoot <- do
               n <- randomInt 0 12
               return (pitchC0 `transpose` n)
             return $ tonality' { root = randomKeyRoot } 
    
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

parseDegrees :: String -> Scale -> [ScaleDegree]
parseDegrees str scale = catMaybes . map (scaleDegreeFromName scale) $ splitBy ',' str

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
      exetype = foldr getExeType "melody" flags
      toneR = foldr getRange mediumToneRange flags
      contextE = foldr getContextEvery 1 flags
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
      getExeType (ExcerciseType str) _ = str
      getExeType _ x = x
      getContextEvery (ContextEvery str) _ = read str
      getContextEvery _ x = x
      
      getRange (Range str) _ = case catMaybes (map maybeRead (splitBy '-' str)) of
        [a,b] -> ToneRange a b
        _ -> error "bad tone range"
      getRange _ x = x
      getKeyRoot (KeyRoot str) x = case pitchFromName str of
        Just p -> p
        _ -> error $ "bad pitch name " ++ str
      getKeyRoot _ x = x

  let degs = parseDegrees degreesstr scaleType
      tonality = Tonality scaleType degs keyRoot toneR
  let runExe q = do
      Stats correct wrong <- withPlayer device $ \p ->
        excercise p (Excercise tonality notesTempo questions pgm contextE) randomKey q 1
      putStrLn $ "CORRECT " ++ show correct ++ " out of " ++ show (correct+wrong)
  case exetype of
    "melody" -> runExe $ randomTonesQuery notes cadence
    "group" -> runExe $ randomToneGroupQuery notes cadence
    "progression" -> runExe $ randomProgressionQuery notes cadence
    s -> error $ "bad exercise: " ++ s
  where
    header = "usage: "
  