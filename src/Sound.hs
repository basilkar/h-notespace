-- Based on tsoding's https://www.youtube.com/watch?v=FYTZkE5BZ-0

module Sound where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable
--import System.Directory
import System.Process
import Text.Printf
import Data.List
import Note

import SoundNote

-- type Pulse = Float
type Seconds = Float
-- type SamplesPerSecond = Float
-- type Hz = Float
type Volume = Float
type Semitones = Float
type BeatsPerMinute = Float

-- melodyA :: [Pulse]
-- melodyA =
--     map (sineWave pitchStandard 0.1 0) [0 .. sampleRate]
--     ++ map (sineWave pitchStandard 0.1 0) [0 .. sampleRate]

melodyB :: [Pulse]
melodyB = SoundNote.envelope (SoundNote.singleNoteSignal SoundNote.defaultSampleRate SoundNote.SoundNote {note = A, freq = 440.0, amp = 0.1, phase = 0, duration = 2}) (0.05, 0.1, 0.8)
--melodyB = SoundNote.singleNoteSignal SoundNote.defaultSampleRate SoundNote.SoundNote {note = A, freq = 440.0, amp = 0.1, phase = 0, duration = 2}

soundFilePath :: FilePath
soundFilePath = "soundfile.bin"

--volume :: Float
--volume = 0.2

--sampleRate :: SamplesPerSecond
--sampleRate = 48000.0

--pitchStandard :: Hz
--pitchStandard = 440.0

-- bpm :: BeatsPerMinute
-- bpm = 120.0

-- beatDuration :: Seconds
-- beatDuration = 60.0 / bpm

-- -- NOTE: the formula is taken from https://pages.mtu.edu/~suits/NoteFreqCalcs.html
-- f :: Semitones -> Hz
-- f n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

-- note :: Semitones -> BeatsPerMinute -> [Pulse]
-- note n beats = freq (f n) (beats * beatDuration)

-- freq :: Hz -> Seconds -> [Pulse]
-- freq hz duration =
--   map (* volume) $ zipWith3 (\x y z -> x * y * z) release attack output
--   where
--     step = (hz * 2 * pi) / sampleRate

--     attack :: [Pulse]
--     attack = map (min 1.0) [0.0,0.001 ..]

--     release :: [Pulse]
--     release = reverse $ take (length output) attack

--     output :: [Pulse]
--     output = map (sin . (* step)) [0.0 .. sampleRate * duration]

-- melody :: [Pulse]
-- melody =
--   concat
--     [ note 0 0.25
--     , note 0 0.25
--     , note 0 0.25
--     , note 0 0.25
--     , note 0 0.5
--     , note 0 0.25
--     , note 0 0.25
--     , note 0 0.25
--     , note 0 0.25
--     , note 0 0.25
--     , note 0 0.25
--     , note 0 0.5
--     , note 5 0.25
--     , note 5 0.25
--     , note 5 0.25
--     , note 5 0.25
--     , note 5 0.25
--     , note 5 0.25
--     , note 5 0.5
--     , note 3 0.25
--     , note 3 0.25
--     , note 3 0.25
--     , note 3 0.25
--     , note 3 0.25
--     , note 3 0.25
--     , note 3 0.5
--     , note (-2) 0.5
--     , note 0 0.25
--     , note 0 0.25
--     , note 0 0.25
--     , note 0 0.25
--     , note 0 0.5
--     ]

-- melodyHehehe :: [Pulse]
-- melodyHehehe = concat [ note 0 0.25
--                 , note 0 0.25
--                 , note 12 0.5
--                 , note 7 (0.5 + 0.25)
--                 , note 6 0.5
--                 , note 5 0.5
--                 , note 3 0.5
--                 , note 0 0.25
--                 , note 3 0.25
--                 , note 5 0.25
--                 ]

save :: FilePath -> IO ()
save filePath = B.writeFile filePath $ B.toLazyByteString $ foldMap B.floatLE melodyB

play :: IO ()
play = do
  save soundFilePath
  _ <- runCommand $ printf "ffplay -autoexit -showmode 1 -f f32le -ar %f %s" SoundNote.defaultSampleRate soundFilePath
--  removeFile soundFilePath
  return ()
