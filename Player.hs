{-# LANGUAGE ScopedTypeVariables #-}
module Player
       (
         Player
       , withPlayer
       , playVoice
       )
       where

import Pitch
import Music

import Data.Word

data Player
   = Player {
     }

withPlayer :: String -> (Player -> IO a) -> IO a
withPlayer device f = do
  f (Player {})
  
playVoice :: Player -> BPM -> Int -> Voice -> IO ()
playVoice p (BPM bpm) pgm v = do
  return ()
