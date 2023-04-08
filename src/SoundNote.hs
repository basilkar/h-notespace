module SoundNote where

import Data.Foldable
import Data.List.Split

import Note

type ADSR = (Float, Float, Float)
type Beats = Float
type BPM = Float
type Envelope = Signal -> ADSR -> Signal
type Hz = Float
type Octave = Int
type Phase = Float
type Pulse = Float
type SamplesPerSecond = Float
type Seconds = Float
type Signal = [Pulse]
type Temperament = Note -> Octave -> Hz
type Volume = Float

data SoundNote = SoundNote
    { note :: Note
    , octave :: Octave
    , amp :: Volume
    , phase :: Phase
    , duration :: Beats
    }
    deriving (Eq, Show, Read)

defaultSampleRate :: SamplesPerSecond
defaultSampleRate = 44000.0

defaultBPM :: BPM
defaultBPM = 120

sineWave :: SamplesPerSecond -> Hz -> Volume -> Phase -> Seconds -> SamplesPerSecond
sineWave sampleRate freq amp phase time = amp * sin (2 * pi * freq * time / sampleRate + phase)

singleNoteSignal :: SamplesPerSecond -> Temperament -> BPM -> SoundNote -> Signal
singleNoteSignal sampleRate temperament bpm soundNote = map (sineWave sampleRate (temperament (note soundNote) (octave soundNote)) (amp soundNote) (phase soundNote)) [0 .. (sampleRate - 1) * (duration soundNote * 60) / bpm]

melodySignal :: SamplesPerSecond -> Temperament -> BPM -> [SoundNote] -> Signal
melodySignal sampleRate temperament bpm [] = []
melodySignal sampleRate temperament bpm [soundNote] = singleNoteSignal sampleRate temperament bpm soundNote
melodySignal sampleRate temperament bpm (soundNote:soundNotes) = singleNoteSignal sampleRate temperament bpm soundNote ++ melodySignal sampleRate temperament bpm soundNotes

defaultEnvelope :: Envelope
defaultEnvelope signal adsr =
    zipWith (*) (stepwiseLinearFilter 0 1 (length attack)) attack
    ++ zipWith (*) (stepwiseLinearFilter 1 0.8 (length decay)) decay
    ++ zipWith (*) (stepwiseLinearFilter 0.8 0.8 (length sustain)) sustain
    ++ zipWith (*) (stepwiseLinearFilter 0.8 0 (length release)) release
        where
            [attack, decay, sustain, release] = adsrSegments signal adsr

{-
 *Sound> defaultEnvelope (take 100 [1, 1 ..]) (0.05, 0.1, 0.8)
 [0.0,0.25,0.5,0.75,1.0,1.0,0.95,0.9,0.85,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.75789475,0.7157895,0.67368424,0.6315789,0.5894737,0.5473684,0.50526315,0.4631579,0.42105263,0.37894738,0.3368421,0.2947368,0.25263155,0.21052629,0.16842103,0.12631577,8.4210515e-2,4.2105258e-2,0.0]
-}

stepwiseLinearFilter :: Float -> Float -> Int -> [Float]
stepwiseLinearFilter start end steps = map (\ x -> (end - start) / fromIntegral (steps - 1) * x + start) [0 .. fromIntegral (steps - 1)]

adsrSegments :: Signal -> ADSR -> [Signal]
adsrSegments signal adsr = splitPlaces [attackLength, decayLength, sustainLength, releaseLength + 1] signal
    where
        (attackEnd, decayEnd, sustainEnd) = adsr
        attackLength = floor (attackEnd * fromIntegral (length signal))
        decayLength = floor (decayEnd * fromIntegral (length signal)) - ceiling (attackEnd * fromIntegral (length signal))
        sustainLength = floor (sustainEnd * fromIntegral (length signal)) - ceiling (decayEnd * fromIntegral (length signal))
        releaseLength = length signal - ceiling (sustainEnd * fromIntegral(length signal))


{-
Examples:

defaultEnvelope (singleNoteSignal defaultSampleRate defaultBPM SoundNote {note = A, freq = 440.0, amp = 0.5, phase = 0, duration = 1}) (0.3, 0.5, 0.7)
-}

standardAPitch :: Hz
standardAPitch = 440

middleCPitch :: Hz
middleCPitch = standardAPitch * (2 ** (1.0 / 12.0)) ** (-9)

equalTemperament :: Temperament
equalTemperament C 4 = middleCPitch
equalTemperament note octave = (equalTemperament C 4 * (2 ** (1.0 / 12.0)) ** m) * 2 ^^ octaveDifference
    where
        m = fromIntegral $ halfstepsDirectedDistance C note
        octaveDifference = octave - 4