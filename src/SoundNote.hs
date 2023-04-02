module SoundNote where

import Data.Foldable
import Data.List.Split

import Note

type Hz = Float
type Phase = Float
type Pulse = Float
type SamplesPerSecond = Float
type Seconds = Float
type Signal = [Pulse]
type Volume = Float

data SoundNote = SoundNote
    { note :: Note
    , freq :: Hz
    , amp :: Volume
    , phase :: Phase
    , duration :: Seconds
    , adsr :: (Float, Float, Float)
    }
    deriving (Eq, Show, Read)

defaultSampleRate :: SamplesPerSecond
defaultSampleRate = 44000.0

sineWave :: SamplesPerSecond -> Hz -> Volume -> Phase -> Seconds -> SamplesPerSecond
sineWave sampleRate freq amp phase time = amp * sin (2 * pi * freq * (time / sampleRate) + phase)

singleNoteSignal :: SamplesPerSecond -> SoundNote -> Signal
singleNoteSignal sampleRate soundNote = map (sineWave sampleRate (freq soundNote) (amp soundNote) (phase soundNote)) [0 .. (sampleRate - 1) * duration soundNote]

melodySignal :: SamplesPerSecond -> [SoundNote] -> Signal
melodySignal sampleRate [] = []
melodySignal sampleRate [soundNote] = singleNoteSignal sampleRate soundNote
melodySignal sampleRate (soundNote:soundNotes) = singleNoteSignal sampleRate soundNote ++ melodySignal sampleRate soundNotes

-- adsr = (0.3, 0.5, 0.7)

envelope :: Signal -> (Float, Float, Float) -> Signal
envelope signal adsr =
    zipWith (*) (stepwiseLinearFilter 0 1 (length attack)) attack
    ++ zipWith (*) (stepwiseLinearFilter 1 0.8 (length decay)) decay
    ++ zipWith (*) (stepwiseLinearFilter 0.8 0.8 (length sustain)) sustain
    ++ zipWith (*) (stepwiseLinearFilter 0.8 0 (length release)) release
        where
            [attack, decay, sustain, release] = adsrSegments signal adsr

stepwiseLinearFilter :: Float -> Float -> Int -> [Float]
stepwiseLinearFilter start end steps = map (\ x -> (end - start) / fromIntegral (steps - 1) * x + start) [0 .. fromIntegral (steps - 1)]

adsrSegments :: Signal -> (Float, Float, Float) -> [Signal]
adsrSegments signal adsr = splitPlaces [attackLength, decayLength, sustainLength, releaseLength + 1] signal
    where
        (attackEnd, decayEnd, sustainEnd) = adsr
        attackLength = floor (attackEnd * fromIntegral (length signal))
        decayLength = floor (decayEnd * fromIntegral (length signal)) - ceiling (attackEnd * fromIntegral (length signal))
        sustainLength = floor (sustainEnd * fromIntegral (length signal)) - ceiling (decayEnd * fromIntegral (length signal))
        releaseLength = length signal - ceiling (sustainEnd * fromIntegral(length signal))


{-
Examples:

envelope (singleNoteSignal defaultSampleRate SoundNote {note = A, freq = 440.0, amp = 0.5, phase = 0, duration = 1, adsr = (0.3, 0.5, 0.7)}) (0.3, 0.5, 0.7)
-}