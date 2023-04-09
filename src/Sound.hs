-- Based on tsoding's https://www.youtube.com/watch?v=FYTZkE5BZ-0

module Sound where

import qualified Data.ByteString.Lazy as B (writeFile)
import qualified Data.ByteString.Builder as B (floatLE, toLazyByteString)
import System.Directory (removeFile)
import System.Process (runCommand, waitForProcess)
import Text.Printf (printf)

import Note
import SoundNote

soundFilePath :: FilePath
soundFilePath = "soundfile.bin"

saveSignalToFile :: Signal -> FilePath -> IO ()
saveSignalToFile signal filePath = B.writeFile filePath $ B.toLazyByteString $ foldMap B.floatLE signal

playSignal :: Signal -> IO ()
playSignal signal = do
  saveSignalToFile signal soundFilePath
  ffplayProcessHandle <- runCommand $ printf "ffplay -autoexit -showmode 1 -f f32le -ar %f %s" defaultSampleRate soundFilePath
  waitForProcess ffplayProcessHandle
  removeFile soundFilePath
  return ()

aMinorSignal = melodyWithDefaultParameters aMinorSoundNotes
cMinorSignal = melodyWithDefaultParameters cMinorSoundNotes
alleMeineEntchenSignal = melodyWithDefaultParameters alleMeineEntchenSoundNotes