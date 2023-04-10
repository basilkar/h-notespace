module Note where

{-
Apart from the auxiliary preexisting datatypes, like Int and [a], the first base datatypes that come to mind regarding this project are an enumeration type for all twelve notes, and the positive reals (or maybe, the positive integers will do in practice, don't know) representing frequencies. Let's concentrate first on the notes. Having notes, we can proceed to define intervals, triads, scales, as values of appropriate higher datatypes.
-}

data Note = A | As | B | C | Cs | D | Ds | E | F | Fs | G | Gs
    deriving (Eq, Enum, Ord, Show, Read, Bounded)

-- Not sure if we'll ever need it, but here's a function turning an arbitrary integer into a note.

toNote :: Int -> Note
toNote m = toEnum $ m `mod` 12

-- An auxiliary function that gives the note m halfsteps above the note r (the "root").

halfsteps :: Note -> Int -> Note
halfsteps r m = toEnum $ (fromEnum r + m) `mod` 12

-- Auxiliary functions that gives the halfsteps distance between two notes; an absolute, a directed, and a change version.

halfstepsMinDistance :: Note -> Note -> Int
halfstepsMinDistance r s = min ((fromEnum r - fromEnum s) `mod` 12) ((fromEnum s - fromEnum r) `mod` 12)

halfstepsMaxDistance :: Note -> Note -> Int
halfstepsMaxDistance r s = max ((fromEnum r - fromEnum s) `mod` 12) ((fromEnum s - fromEnum r) `mod` 12)

halfstepsDistance :: Note -> Note -> Int
halfstepsDistance = halfstepsMinDistance

halfstepsDirectedDistance :: Note -> Note -> Int
halfstepsDirectedDistance r s = (fromEnum s - fromEnum r) `mod` 12

clockwiseChange :: Note -> Note -> Int
clockwiseChange m n = (fromEnum n + 12 - fromEnum m) `mod` 12

counterclockwiseChange :: Note -> Note -> Int
counterclockwiseChange m n = - (clockwiseChange n m)

minChange :: Note -> Note -> Int
minChange m n
  | absdiff = clock
  | otherwise = counterclock
    where
      absdiff = abs clock <= abs counterclock
      clock = clockwiseChange m n
      counterclock = counterclockwiseChange m n

-- A signature is a list of integers representing intervals in halfsteps above a given root. Given a root r and a signature sig define the list of notes that are each that many halfsteps apart from the root according to the signature. NB: this is meant to work for non-negative integers for the time being, the intended use being the generation of scales and chords, not of melody.

type Sig = [Int]

notesBySignature :: Note -> [Int] -> [Note]
notesBySignature r sig = [r `halfsteps` m | m <- sig]

-- Hardcoding signatures

sigInterval1p :: [Int] -- unison interval signature
sigInterval1p = [0,0]

sigInterval2m :: [Int] -- minor second interval signature
sigInterval2m = [0,1]

sigInterval2M :: [Int] -- major second interval signature
sigInterval2M = [0,2]

sigInterval3m :: [Int] -- minor third interval signature
sigInterval3m = [0,3]

sigInterval3M :: [Int] -- major third interval signature
sigInterval3M = [0,4]

sigInterval4p :: [Int] -- perfect fourth interval signature
sigInterval4p = [0,5]

sigInterval4a :: [Int] -- augmented fourth interval signature
sigInterval4a = [0,6]

sigInterval5p :: [Int] -- perfect fifth interval signature
sigInterval5p = [0,7]

sigInterval6m :: [Int] -- minor sixth interval signature
sigInterval6m = [0,8]

sigInterval6M :: [Int] -- major sixth interval signature
sigInterval6M = [0,9]

sigInterval7m :: [Int] -- minor seventh interval signature
sigInterval7m = [0,10]

sigInterval7M :: [Int] -- major seventh interval signature
sigInterval7M = [0,11]

sigTriadM :: [Int] -- major triad signature
sigTriadM = [0,4,7]

sigTriadm :: [Int] -- minor triad signature
sigTriadm = [0,3,7]

sigTriada :: [Int] -- augmented triad signature
sigTriada = [0,4,8]

sigTriadd :: [Int] -- diminished triad signature
sigTriadd = [0,3,6]

sig7ChordMM :: [Int] -- major seventh chord
sig7ChordMM = sigTriadM ++ [11]

sig7ChordMm :: [Int] -- seventh chord
sig7ChordMm = sigTriadM ++ [10]

sig7ChordmM :: [Int] -- minor major seventh chord
sig7ChordmM = sigTriadm ++ [11]

sig7Chordmm :: [Int] -- minor seventh chord
sig7Chordmm = sigTriadm ++ [10]

sig7Chorddm :: [Int] -- half-diminished chord
sig7Chorddm = sigTriadd ++ [10]

sig7Chordam :: [Int] -- augmented seventh chord
sig7Chordam = sigTriada ++ [10]

sig6ChordMM :: [Int] -- major sixth chord
sig6ChordMM = sigTriadM ++ [9]

sig6ChordmM :: [Int] -- minor sixth chord
sig6ChordmM = sigTriadm ++ [9]

sigScaleM :: [Int] -- major scale signature
sigScaleM = [0,2,4,5,7,9,11]

sigScalemh :: [Int] -- harmonic minor scale signature
sigScalemh = [0,2,3,5,7,8,11]

sigScaleMh :: [Int] -- harmonic major scale signature
sigScaleMh = [0,2,4,5,7,8,11]

sigScalemm :: [Int] -- melodic minor scale signature
sigScalemm = [0,2,3,5,7,9,11]

sigScaledh :: [Int] -- double harmonic scale signature
sigScaledh = [0,1,4,5,7,8,11]

sigScalesd :: [Int] -- symmetric diminished scale signature
sigScalesd = [0,2,3,5,6,8,9,11]

sigScalec :: [Int] -- chromatic scale signature
sigScalec = [0..11]

sigScalewt :: [Int] -- whole tone scale signature
sigScalewt = [0,2,4,6,8,10]

sigScalepM :: [Int] -- pentatonic major scale signature
sigScalepM = [0,2,4,7,9]

sigScalepm :: [Int] -- pentatonic minor scale signature
sigScalepm = [0,3,5,7,10]

-- Hardcoding intervals

interval1p :: Note -> [Note] -- unison
interval1p r = notesBySignature r [0,0]

interval2m :: Note -> [Note] -- minor second
interval2m r = notesBySignature r [0,1]

interval2M :: Note -> [Note] -- major second
interval2M r = notesBySignature r [0,2]

interval3m :: Note -> [Note] -- minor third
interval3m r = notesBySignature r [0,3]

interval3M :: Note -> [Note] -- major third
interval3M r = notesBySignature r [0,4]

interval4p :: Note -> [Note] -- perfect fourth
interval4p r = notesBySignature r [0,5]

interval4a :: Note -> [Note] -- augmented fourth
interval4a r = notesBySignature r [0,6]

interval5p :: Note -> [Note] -- perfect fifth
interval5p r = notesBySignature r [0,7]

interval6m :: Note -> [Note] -- minor sixth
interval6m r = notesBySignature r [0,8]

interval6M :: Note -> [Note] -- major sixth
interval6M r = notesBySignature r [0,9]

interval7m :: Note -> [Note] -- minor seventh
interval7m r = notesBySignature r [0,10]

interval7M :: Note -> [Note] -- major seventh
interval7M r = notesBySignature r [0,11]

-- triads

triadM :: Note -> [Note] -- major triad
triadM r = notesBySignature r [0,4,7]

triadm :: Note -> [Note] -- minor triad
triadm r = notesBySignature r [0,3,7]

triada :: Note -> [Note] -- augmented triad
triada r = notesBySignature r [0,4,8]

triadd :: Note -> [Note] -- diminished triad
triadd r = notesBySignature r [0,3,6]

-- and scales

scaleM :: Note -> [Note] -- major scale
scaleM r = notesBySignature r [0,2,4,5,7,9,11]

scalemh :: Note -> [Note] -- harmonic minor scale
scalemh r = notesBySignature r [0,2,3,5,7,8,11]

scaleMh :: Note -> [Note] -- harmonic major scale
scaleMh r = notesBySignature r [0,2,4,5,7,8,11]

scalemm :: Note -> [Note] -- melodic minor scale
scalemm r = notesBySignature r [0,2,3,5,7,9,11]

scalec :: Note -> [Note] -- chromatic scale
scalec r = notesBySignature r [0..11]

scalewt :: Note -> [Note] -- whole tone scale
scalewt r = notesBySignature r [0,2,4,6,8,10]

scalepM :: Note -> [Note] -- pentatonic major scale
scalepM r = notesBySignature r [0,2,4,7,9]

scalepm :: Note -> [Note] -- pentatonic minor scale
scalepm r = notesBySignature r [0,3,5,7,10]

-- Define the signature of a given note-list as the list of the respective directed halfstep-distances.

signatureByNotes :: [Note] -> [Int]
signatureByNotes ns = [halfstepsDirectedDistance (head ns) n | n <- ns]

-- In order to get the modes of a given scale, or the inversions of a given chord, we first define the cyclic permutations of a (finite) list.

cyclicPermutation :: Int -> [a] -> [a]
cyclicPermutation _ []     = []
cyclicPermutation m xs     = take l (drop m cxs)
    where cxs = cycle xs
          l = length xs

-- Now, according to established terminology, the m-th mode of a scale sc with root r, should be the (m-1)-th cyclic permutation of the given scale at root r, while, if we're thinking about chords, the m-th inversion should be the m-th cyclic permutation of a given chord (the 0-th inversion is called "root position"); what the heck, we define both.

mode :: Int -> (Note -> [Note]) -> Note -> [Note]
mode m sc r = cyclicPermutation (m-1) (sc r)

inversion :: Int -> (Note -> [Note]) -> Note -> [Note]
inversion m sc r = cyclicPermutation m (sc r)

-- We relativize the above to an arbitrary rerooting: rsc stands now for a scale rooted at r and s stands for the new root.

modeAtRoot :: Int -> (Note -> [Note]) -> Note -> Note -> [Note]
modeAtRoot m rsc r s = notesBySignature s (signatureByNotes (mode m rsc r))

inversionAtRoot :: Int -> (Note -> [Note]) -> Note -> Note -> [Note]
inversionAtRoot m rsc r s = notesBySignature s (signatureByNotes (inversion m rsc r))

-- We can obtain the abstract signatures of modes by the following.

sigMode :: Int -> [Int] -> [Int]
sigMode m sig = signatureByNotes $ notesBySignature A (cyclicPermutation (m-1) sig) -- the choice of A is arbitrary, any note will do