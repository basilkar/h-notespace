-- Based on tsoding's https://www.youtube.com/watch?v=FYTZkE5BZ-0

module Sound where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Control.Concurrent
import Data.Foldable
import System.Directory
import System.Process
import Text.Printf
import Data.List
import Note

import SoundNote

type Seconds = Float
type Volume = Float
type Semitones = Float
type BeatsPerMinute = Float

melodyB :: Signal
melodyB = SoundNote.envelope (SoundNote.singleNoteSignal SoundNote.defaultSampleRate SoundNote.equalTemperament SoundNote.defaultBPM SoundNote.SoundNote {note = A, amp = 0.1, phase = 0, duration = 1}) (0.05, 0.1, 0.8) 
  ++ SoundNote.envelope (SoundNote.singleNoteSignal SoundNote.defaultSampleRate SoundNote.equalTemperament SoundNote.defaultBPM SoundNote.SoundNote {note = B, amp = 0.1, phase = 0, duration = 1}) (0.05, 0.1, 0.8)
  ++ SoundNote.envelope (SoundNote.singleNoteSignal SoundNote.defaultSampleRate SoundNote.equalTemperament SoundNote.defaultBPM SoundNote.SoundNote {note = C, amp = 0.1, phase = 0, duration = 1}) (0.05, 0.1, 0.8)
  ++ SoundNote.envelope (SoundNote.singleNoteSignal SoundNote.defaultSampleRate SoundNote.equalTemperament SoundNote.defaultBPM SoundNote.SoundNote {note = D, amp = 0.1, phase = 0, duration = 1}) (0.05, 0.1, 0.8)
  ++ SoundNote.envelope (SoundNote.singleNoteSignal SoundNote.defaultSampleRate SoundNote.equalTemperament SoundNote.defaultBPM SoundNote.SoundNote {note = E, amp = 0.1, phase = 0, duration = 1}) (0.05, 0.1, 0.8)
  ++ SoundNote.envelope (SoundNote.singleNoteSignal SoundNote.defaultSampleRate SoundNote.equalTemperament SoundNote.defaultBPM SoundNote.SoundNote {note = F, amp = 0.1, phase = 0, duration = 1}) (0.05, 0.1, 0.8)
  ++ SoundNote.envelope (SoundNote.singleNoteSignal SoundNote.defaultSampleRate SoundNote.equalTemperament SoundNote.defaultBPM SoundNote.SoundNote {note = G, amp = 0.1, phase = 0, duration = 1}) (0.05, 0.1, 0.8)

soundFilePath :: FilePath
soundFilePath = "soundfile.bin"

saveSignalToFile :: Signal -> FilePath -> IO ()
saveSignalToFile signal filePath = B.writeFile filePath $ B.toLazyByteString $ foldMap B.floatLE signal

play :: IO ()
play = do
  saveSignalToFile melodyB soundFilePath
  _ <- runCommand $ printf "ffplay -autoexit -showmode 1 -f f32le -ar %f %s" SoundNote.defaultSampleRate soundFilePath
--  removeFile soundFilePath
  return ()
