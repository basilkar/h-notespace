-- Based on tsoding's https://www.youtube.com/watch?v=FYTZkE5BZ-0
module Player where

import qualified Data.ByteString.Lazy as B (writeFile)
import qualified Data.ByteString.Builder as B (floatLE, toLazyByteString)
import System.Directory (removeFile)
import System.Process (runCommand, waitForProcess)
import Text.Printf (printf)

import IOUtils
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

player :: IO ()
player = do
  putStrLn "PLAYER: Input a melody that you want to play in note-octave-duration triples (the duration given in number of beats):"
  let inputs = []
  nods <- charInputsToNods inputs
  playSignal $ melodyWithDefaultParameters (map nodToSoundNote nods)
