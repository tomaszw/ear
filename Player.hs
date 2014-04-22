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

import Pitch
import Music

import Control.Monad.Trans.Cont (ContT(ContT), runContT )
import Control.Monad.IO.Class (liftIO, )
import Control.Monad (zipWithM_, )
import Control.Concurrent
import Data.Word

data Player = Player (SndSeq.T SndSeq.DuplexMode) Port.T Queue.T Connect.T
newtype EventTime = EventTime Int deriving (Eq,Ord)

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
        f (Player h p q conn)

event (Player h p q conn) ev t =
  Event.output h $
    (Event.forConnection conn ev) { Event.queue = q, Event.time = Time.consAbs (Time.Tick $ timeConv t) }
  
queueNote :: Player -> (Pitch, Time, Time) -> IO ()
queueNote player@(Player h p q conn) (pitch, t0, t1) = do
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

playVoice :: Player -> BPM -> Voice -> IO ()
playVoice p@(Player h _ q conn) (BPM bpm) v = do
  Queue.control h q Event.QueueStart Nothing
  Queue.control h q (Event.QueueTempo (Event.Tempo (60000000 `div` fromIntegral bpm))) Nothing
  event p (Event.CtrlEv Event.PgmChange $ Event.Ctrl (Event.Channel 0) (Event.Parameter 0) (Event.Value 0)) 0
  
  mapM_ (queueNote p) notes
  echo p endTime
  _ <- Event.drainOutput h
  _ <- Event.outputPending h
  waitForEcho p
  where
    notes = notesFromVoice v
    endTime = foldr (\(p,t0,t1) t -> max t t1) 0 notes
    
echo :: Player -> Time -> IO ()
echo (Player h p q conn) time = do
  c <- Client.getId h
  let me = Addr.Cons c p
      ev = Event.CustomEv Event.Echo $ Event.Custom 0 0 0
  Event.output h $ (Event.forConnection conn ev) { Event.dest = me
                                                 , Event.queue = q
                                                 , Event.time = Time.consAbs (Time.Tick $ timeConv time) }
  return ()

waitForEcho pl@(Player h p q conn) = do
  c <- Client.getId h
  let me = Addr.Cons c p
  event <- Event.input h
  case Event.body event of
    Event.CustomEv Event.Echo _d ->
      if Event.source event == me
      then return ()
      else waitForEcho pl

