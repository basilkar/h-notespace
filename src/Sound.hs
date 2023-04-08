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

defaultADSR = (0.05, 0.1, 0.8)

melody :: Signal
melody = melodyFromSoundNotes defaultEnvelope defaultADSR defaultSampleRate equalTemperament defaultBPM soundNotes

melodyFromSoundNotes :: Envelope -> ADSR -> SamplesPerSecond -> Temperament -> BPM -> [SoundNote] -> Signal
melodyFromSoundNotes envelope adsr sampleRate temperament bpm [] = []
melodyFromSoundNotes envelope adsr sampleRate temperament bpm [soundNote] =
  envelope (singleNoteSignal sampleRate temperament bpm soundNote) adsr
melodyFromSoundNotes envelope adsr sampleRate temperament bpm (soundNote:soundNotes) =
  melodyFromSoundNotes envelope adsr sampleRate temperament bpm [soundNote] ++ melodyFromSoundNotes envelope adsr sampleRate temperament bpm soundNotes

soundNotes = [
  SoundNote {note = A, octave = 3, amp = 0.1, phase = 0, duration = 1},
  SoundNote {note = B, octave = 3, amp = 0.1, phase = 0, duration = 1},
  SoundNote {note = C, octave = 4, amp = 0.1, phase = 0, duration = 1},
  SoundNote {note = D, octave = 4, amp = 0.1, phase = 0, duration = 1},
  SoundNote {note = E, octave = 4, amp = 0.1, phase = 0, duration = 1},
  SoundNote {note = F, octave = 4, amp = 0.1, phase = 0, duration = 1},
  SoundNote {note = G, octave = 4, amp = 0.1, phase = 0, duration = 1},
  SoundNote {note = A, octave = 4, amp = 0.1, phase = 0, duration = 1},
  SoundNote {note = B, octave = 4, amp = 0.1, phase = 0, duration = 1},
  SoundNote {note = C, octave = 5, amp = 0.1, phase = 0, duration = 1},
  SoundNote {note = D, octave = 5, amp = 0.1, phase = 0, duration = 1},
  SoundNote {note = E, octave = 5, amp = 0.1, phase = 0, duration = 1},
  SoundNote {note = F, octave = 5, amp = 0.1, phase = 0, duration = 1},
  SoundNote {note = G, octave = 5, amp = 0.1, phase = 0, duration = 1},
  SoundNote {note = A, octave = 5, amp = 0.1, phase = 0, duration = 1}
  ]

soundFilePath :: FilePath
soundFilePath = "soundfile.bin"

saveSignalToFile :: Signal -> FilePath -> IO ()
saveSignalToFile signal filePath = B.writeFile filePath $ B.toLazyByteString $ foldMap B.floatLE signal

play :: IO ()
play = do
  saveSignalToFile melody soundFilePath
  _ <- runCommand $ printf "ffplay -autoexit -showmode 1 -f f32le -ar %f %s" defaultSampleRate soundFilePath
--  removeFile soundFilePath
  return ()
