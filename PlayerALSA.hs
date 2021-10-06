{-# LANGUAGE ScopedTypeVariables #-}
module Player
       (
         Player
       , withPlayer
       , playVoice
       )
       where

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer.Queue as Queue
import qualified Sound.ALSA.Sequencer.Time as Time
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Exception as AlsaExc
import qualified Control.Concurrent.Event as CCEv
import Pitch
import Music

import Control.Monad.Trans.Cont (ContT(ContT), runContT )
import Control.Monad.IO.Class (liftIO, )
import Control.Monad (zipWithM_, forever, void)
import Control.Concurrent
import Data.Word

data Player
   = Player {
       pSeq :: SndSeq.T SndSeq.DuplexMode
     , pPort :: Port.T
     , pQue :: Queue.T
     , pConn :: Connect.T
     , pI :: MVar Word32
     , pEv :: CCEv.Event
     , pEchos :: MVar [Word32]
     }

newtype EventTime = EventTime Int deriving (Eq,Ord)

timeEq (Time.Cons Time.Absolute (Time.Tick t1))
       (Time.Cons Time.Absolute (Time.Tick t2))
  = t1 == t2
timeEq _ _ = False

withPlayer :: String -> (Player -> IO a) -> IO a
withPlayer device f = do
  SndSeq.withDefault SndSeq.Block $ \(h :: SndSeq.T SndSeq.DuplexMode) -> do
    Client.setName h "eartrainer"
    Port.withSimple h "out"
          (Port.caps [Port.capRead, Port.capSubsRead, Port.capWrite])
          (Port.types [Port.typeMidiGeneric, Port.typeApplication])
          $ \p -> do
      Queue.with h $ \q -> do
        c <- Client.getId h
        let me = Addr.Cons c p
        conn <- Connect.createTo h p =<< Addr.parse h device
        Queue.control h q Event.QueueStart Nothing
        ev <- CCEv.new
        es <- newMVar []
        i  <- newMVar 0
        let pl = Player h p q conn i ev es
        forkOS $ consumeEvents pl
        f pl

event (Player h p q conn _ _ _) ev t =
  Event.output h $
    (Event.forConnection conn ev)
        { Event.queue = q
        , Event.time  = Time.consAbs (Time.Tick $ timeConv t) }
  
queueNote :: Player -> (Pitch, Time, Time) -> IO ()
queueNote player (pitch, t0, t1) = do
  let on = Event.NoteEv Event.NoteOn $
             Event.simpleNote
               (Event.Channel 0)
               (Event.Pitch $ pitchValue pitch)
               (Event.Velocity 127)
      off = Event.NoteEv Event.NoteOff $
              Event.simpleNote
                (Event.Channel 0)
                (Event.Pitch $ pitchValue pitch)
                Event.normalVelocity

  event player on t0
  event player off t1
  return ()

queueNotes :: Player -> [(Pitch, Time, Time)] -> IO ()
queueNotes p notes = mapM_ (queueNote p) notes

timeConv :: Time -> Word32
timeConv t = round (t * 96)

nextI :: Player -> IO Word32
nextI p = modifyMVar (pI p) (\i -> return (i+1, i))

playVoice :: Player -> BPM -> Int -> Voice -> IO ()
playVoice p (BPM bpm) pgm v = do
  Queue.control h q Event.QueueStart Nothing
  Queue.control h q (Event.QueueTempo (Event.Tempo (60000000 `div` fromIntegral bpm))) Nothing
  event p
        (Event.CtrlEv Event.PgmChange $
           Event.Ctrl
            (Event.Channel 0)
            (Event.Parameter 0)
            (Event.Value $ fromIntegral pgm))
        0
  
  mapM_ (queueNote p) notes
  i <- nextI p
  echo p endTime i
  _ <- Event.drainOutput h
  _ <- Event.outputPending h
  waitForEcho p i
  where
    notes = notesFromVoice v
    endTime = voiceDuration v
    h = pSeq p
    q = pQue p

echo :: Player -> Time -> Word32 -> IO ()
echo p time x = do
  c <- Client.getId h
  let me = Addr.Cons c port
      ev = Event.CustomEv Event.Echo $ Event.Custom x 0 0
  void . Event.output h
    $ (Event.forConnection conn ev)
                     { Event.dest = me
                     , Event.queue = q
                     , Event.time = Time.consAbs (Time.Tick $ timeConv time) }
  where
    h = pSeq p
    port = pPort p
    q = pQue p
    conn = pConn p

waitForEcho :: Player -> Word32 -> IO ()
waitForEcho p e = do
  x <- test
  case x of
    True -> return ()
    _    -> CCEv.wait (pEv p) >> waitForEcho p e
  where
    test = modifyMVar (pEchos p) $
           \es -> return $
                  if e `elem` es
                  then (dropFirst e es, True)
                  else (es, False)
    dropFirst x [] = []
    dropFirst x (y:ys)
      | x == y    = ys
      | otherwise = y : dropFirst x ys

consumeEvents :: Player -> IO ()
consumeEvents p@(Player h port q conn _ ev echos) = do
  c <- Client.getId h
  let me = Addr.Cons c port
  forever $ do
    event <- Event.input h
    case Event.body event of
      Event.CustomEv Event.Echo (Event.Custom x _ _) -> do
        modifyMVar_ echos (\xs -> return (x:xs))
        CCEv.signal ev
      _ -> putStrLn ("event: " ++ show event)
