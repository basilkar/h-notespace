module Lib where

import Data.List -- for sorting lists, for nub
import Math.Combinat -- for rhythmic exercises
import System.Random -- for the guitar fretboard quizzes

import Utils

{--------------------------------------}
{- 1 -- AN ENUMERATION TYPE FOR NOTES -}
{--------------------------------------}

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
halfstepsMinDistance r s = min (((fromEnum r) - (fromEnum s)) `mod` 12) (((fromEnum s) - (fromEnum r)) `mod` 12)

halfstepsMaxDistance :: Note -> Note -> Int
halfstepsMaxDistance r s = max (((fromEnum r) - (fromEnum s)) `mod` 12) (((fromEnum s) - (fromEnum r)) `mod` 12)

halfstepsDistance :: Note -> Note -> Int
halfstepsDistance = halfstepsMinDistance

halfstepsDirectedDistance :: Note -> Note -> Int
halfstepsDirectedDistance r s = ((fromEnum s) - (fromEnum r)) `mod` 12

clockwiseChange :: Note -> Note -> Int
clockwiseChange m n = ((fromEnum n) + 12 - (fromEnum m)) `mod` 12

counterclockwiseChange :: Note -> Note -> Int
counterclockwiseChange m n = - (clockwiseChange n m)

minChange :: Note -> Note -> Int
minChange m n
  | absdiff = clock
  | otherwise = counterclock
    where
      absdiff = (abs clock) <= (abs counterclock)
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

sig7Chordmm :: [Int] -- minor major seventh chord
sig7Chordmm = sigTriadm ++ [11]

sig7ChordmM :: [Int] -- minor seventh chord
sig7ChordmM = sigTriadm ++ [10]

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
cyclicPermutation m []     = []
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

{---------------------------------------}
{- 1.1 -- APPLICATION: IMPRO SUGGESTER -}
{---------------------------------------}

{-

Suggest scales fitting to a set of notes.

INPUT a note list metric, a list of notes c and a list of lists of notes cs
OUTPUT the (c:cs) sorted by the given metric

-}

sortByMetric :: ([Note] -> [Note] -> Int) -> [[Note]] -> [Note] -> [[Note]]
sortByMetric dist cs c = c : sortOn (dist c) cs

scaleSuggester :: [Note] -> Note -> [[Note]]
scaleSuggester ns r = sortByMetric levenshtein allscales ns
    where allscales = nub $ [modeAtRoot m scaleM r r | m <- [1..7]] ++ [modeAtRoot m scalepM r r | m <- [1..5]] ++ [modeAtRoot m scalewt r r | m <- [1..6]] ++ [modeAtRoot m scalemh r r | m <- [1..7]] ++ [modeAtRoot m scalemm r r | m <- [1..7]] ++ [modeAtRoot m scalec r r | m <- [1..12]] ++ [modeAtRoot m scaleMh r r | m <- [1..7]] -- this is still ugly

triadSuggester :: [Note] -> Note -> [[Note]]
triadSuggester ns r = sortByMetric levenshtein alltriads ns
    where alltriads = nub $ [inversionAtRoot m triadM r r | m <- [1..3]] ++ [inversionAtRoot m triadm r r | m <- [1..3]] ++ [inversionAtRoot m triadd r r | m <- [1..3]] ++ [inversionAtRoot m triada r r | m <- [1..3]]

{- EXAMPLES

> levenshtein (mode 6 scaleM C) (scalemh A)
1

> levenshtein (mode 6 scaleM C) (scalemm A)
2

> halfstepsDistance A B
2

> halfstepsDistance B A
2

> sortByMetric levenshtein [mode m scaleM C | m <- [1..7]] (yieldOfChord c_major_extended)
[[C,E,G],[C,D,E,F,G,A,B],[A,B,C,D,E,F,G],[B,C,D,E,F,G,A],[D,E,F,G,A,B,C],[G,A,B,C,D,E,F],[E,F,G,A,B,C,D],[F,G,A,B,C,D,E]]

> sortByMetric levenshtein [modeAtRoot m scaleM C C | m <- [1..7]] (yieldOfChord c_major_extended)
[[C,E,G],[C,D,E,F,G,A,B],[C,D,E,Fs,G,A,B],[C,D,E,F,G,A,As],[C,D,Ds,F,G,A,As],[C,Cs,Ds,F,G,Gs,As],[C,D,Ds,F,G,Gs,As],[C,Cs,Ds,F,Fs,Gs,As]]

> sortByMetric levenshtein [modeAtRoot m scalemm C C | m <- [1..7]] (yieldOfChord c_major_extended)
[[C,E,G],[C,D,E,Fs,G,A,As],[C,D,E,F,G,Gs,As],[C,D,Ds,F,G,A,B],[C,Cs,Ds,F,G,A,As],[C,D,E,Fs,Gs,A,B],[C,Cs,Ds,E,Fs,Gs,As],[C,D,Ds,F,Fs,Gs,As]]

> scaleSuggester [C,G,E] C
[[C,G,E],[C,D,E,G,A],[C,D,F,G,As],[C,D,F,G,A],[C,Ds,F,G,As],[C,Ds,F,Gs,As],[C,D,E,Fs,Gs,As],[C,D,E,F,G,A,B],[C,D,Ds,F,G,A,As],[C,Cs,Ds,F,G,Gs,As],[C,D,E,Fs,G,A,B],[C,D,E,F,G,A,As],[C,D,Ds,F,G,Gs,As],[C,D,Ds,F,G,Gs,B],[C,D,E,F,Gs,A,B],[C,D,Ds,Fs,G,A,As],[C,Cs,E,F,G,Gs,As],[C,Ds,E,Fs,G,A,B],[C,Cs,Ds,E,Fs,Gs,A],[C,D,Ds,F,G,A,B],[C,Cs,Ds,F,G,A,As],[C,D,E,Fs,Gs,A,B],[C,D,E,Fs,G,A,As],[C,D,E,F,G,Gs,As],[C,Cs,Ds,E,Fs,Gs,As],[C,Cs,Ds,F,Fs,Gs,As],[C,Cs,Ds,F,Fs,A,As],[C,D,Ds,F,Fs,Gs,As],[C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B]]

> scaleSuggester [C,G,E] A
[[C,G,E],[A,C,D,E,G],[A,B,Cs,E,Fs],[A,B,D,E,G],[A,C,D,F,G],[A,B,D,E,Fs],[A,B,C,D,E,Fs,G],[A,As,C,D,E,F,G],[A,B,C,D,E,F,G],[A,B,C,D,E,F,Gs],[A,B,C,Ds,E,Fs,G],[A,C,Cs,Ds,E,Fs,Gs],[A,B,C,D,E,Fs,Gs],[A,As,C,D,E,Fs,G],[A,B,Cs,D,E,Fs,Gs],[A,B,Cs,Ds,E,Fs,Gs],[A,B,Cs,D,E,Fs,G],[A,As,C,D,Ds,F,G],[A,B,Cs,Ds,F,G],[A,As,C,D,Ds,Fs,G],[A,As,Cs,D,E,F,G],[A,As,C,Cs,Ds,F,Fs],[A,B,Cs,Ds,E,Fs,G],[A,B,Cs,D,E,F,G],[A,B,C,D,Ds,F,G],[A,As,C,Cs,Ds,F,G],[A,B,Cs,D,F,Fs,Gs],[A,B,Cs,Ds,F,Fs,Gs],[A,As,B,C,Cs,D,Ds,E,F,Fs,G,Gs]]

> triadSuggester [C,G,E] C
[[C,G,E],[C,Ds,Gs],[C,F,A],[C,E,G],[C,E,A],[C,F,Gs],[C,Ds,G],[C,Ds,A],[C,Fs,A],[C,Ds,Fs],[C,E,Gs]]

> triadSuggester [C,G,E] A
[[C,G,E],[A,Cs,E],[A,C,E],[A,C,F],[A,D,Fs],[A,Cs,Fs],[A,D,F],[A,C,Fs],[A,Ds,Fs],[A,C,Ds],[A,Cs,F]]

Note that the Levenshtein distance needs adaptation to fit some musical intuitions. For example, here is what we get for the song "Etymology":

-}

etymology_solo_suggestions = scaleSuggester (sort $ nub $ [E,G,D,C,Ds,G,A,C,E,G,Ds,G]) E

{-
> etymology_solo_suggestions
[[A,C,D,Ds,E,G],[E,G,A,C,D],[E,Fs,Gs,B,Cs],[E,Fs,A,B,D],[E,Fs,A,B,Cs],[E,G,A,B,D],[E,Fs,Gs,As,C,D],[E,Fs,Gs,A,C,Cs,Ds],[E,Fs,Gs,A,B,Cs,Ds],[E,Fs,G,A,B,Cs,D],[E,F,G,A,B,C,D],[E,Fs,Gs,As,B,Cs,Ds],[E,Fs,Gs,A,B,Cs,D],[E,Fs,G,A,B,C,D],[E,F,G,A,As,C,D],[E,Fs,G,A,B,C,Ds],[E,F,G,A,As,Cs,D],[E,Fs,G,As,B,Cs,D],[E,F,Gs,A,B,C,D],[E,G,Gs,As,B,Cs,Ds],[E,F,G,Gs,As,C,Cs],[E,Fs,G,A,B,Cs,Ds],[E,F,G,A,B,Cs,D],[E,Fs,Gs,As,C,Cs,Ds],[E,Fs,Gs,As,B,Cs,D],[E,Fs,Gs,A,B,C,D],[E,Fs,G,A,As,C,D],[E,F,G,Gs,As,C,D],[E,F,Fs,G,Gs,A,As,B,C,Cs,D,Ds]]

> length etymology_solo_suggestions
29

E aeolian flat five (aka half diminished, aka locrian natural 2) is indeed among these suggestions.

-}

e_aeolian_flat_five = mode 6 scalemm G

{-

> elem e_aeolian_flat_five etymology_solo_suggestions
True

But nowhere close to the top ones.

> elemIndex e_aeolian_flat_five etymology_solo_suggestions
Just 26

Indeed, E major comes before it!

> elemIndex (scaleM E) etymology_solo_suggestions
Just 8

In adapting the Levenshtein distance we could think about giving weighs to particular notes (the notes of the root triad, if nothing else).

-}

{------------------------------------}
{- 1.2 -- APPLICATION: SCALE CHORDS -}
{------------------------------------}

{-

Produce the chords of a given scale, according to a given signature.

INPUT a scale and a signature
OUTPUT a list of lists of notes from the given scale that fit the given signature

-}

scalechords :: [Note] -> [Int] -> [[Note]]
scalechords [] _ = []
scalechords _ [] = []
scalechords sc sig = [ notesBySignature (sc !! 0) c | c <- memoArray, all (\ n -> elem n scsig) c]
    where
        scsig = signatureByNotes sc
        memoArray = map (\ m -> (map (\ n -> (n + m) `mod` 12) sig)) scsig

{- EXAMPLES -}

c_major_scale = scaleM C -- alternatively, c_major_scale = notesBySignature C sigScaleM

{-

The following find all major, minor, augmented, and diminished triads to be found in the key of C major.

> scalechords c_major_scale sigTriadM
[[C,E,G],[F,A,C],[G,B,D]]

> scalechords c_major_scale sigTriadm
[[D,F,A],[E,G,B],[A,C,E]]

> scalechords c_major_scale sigTriada
[]

> scalechords c_major_scale sigTriadd
[[B,D,F]]

We can find all (traditional) triads to be found in the key of C major

> map (\ sigs -> scalechords c_major_scale sigs) [sigTriadM, sigTriadm, sigTriada, sigTriadd]
[[[C,E,G],[F,A,C],[G,B,D]],[[D,F,A],[E,G,B],[A,C,E]],[],[[B,D,F]]]

or in the scale of E minor pentatonic

> map (\ sigs -> scalechords (notesBySignature E sigScalepm) sigs) [sigTriadM, sigTriadm, sigTriada, sigTriadd]
[[[G,B,D]],[[E,G,B]],[],[]]

and in the same fashion we can find all (traditional) seventh chords in these keys

> map (\ sigs -> scalechords c_major_scale sigs) [sig7ChordMM, sig7ChordMm, sig7Chordmm, sig7ChordmM, sig7Chorddm, sig7Chordam]

The same example for, say, the E harmonic major:

-}

e_harmonic_major = scaleMh E

{-

> map (\ sigs -> scalechords e_harmonic_major sigs) [sig7ChordMM, sig7ChordMm, sig7Chordmm, sig7ChordmM, sig7Chorddm, sig7Chordam]
[[[E,Gs,B,Ds]],[[Gs,C,Ds,Fs],[B,Ds,Fs,A]],[[A,C,E,Gs]],[[Gs,B,Ds,Fs]],[[Fs,A,C,E]],[[Gs,C,E,Fs]]]

Similarly, we can use the function scalechords to find all perfect fourth intervals, say, of A minor pentatonic.mode 6 scalemm G

> map (\ sigs -> scalechords (notesBySignature E sigScalepm) sigs) [ sigInterval4p ]
[[[E,A],[A,D],[B,E],[D,G]]]

and, to contrast, all perfect fourth intervals of C major:

> map (\ sigs -> scalechords c_major_scale sigs) [ sigInterval4p ]
[[[C,F],[D,G],[E,A],[G,C],[A,D],[B,E]]]

Finally, all tritones in A melodic minor.

> map (\ sigs -> scalechords (notesBySignature A sigScalemm) sigs) [ sigInterval4a ]
[[[C,Fs],[D,Gs],[Fs,C],[Gs,D]]]

Also, we can find existing instances of a scale or mode within a given scale. For example, this is how we find which notes of E major key constitute its locrian mode (the seventh mode of a major scale).

> scalechords (scaleM E) (signatureByNotes [B, C, D, E, F, G, A])

-}

{-------------------------------------------------------}
{- 1.3 -- APPLICATION: INTERVAL-CHORD-SCALE RECOGNIZER -}
{-------------------------------------------------------}

{-

Recognize a rooted sequence of notes (meaning, a sequence of notes together with a note that is to be perceived as the root of the musical pattern) as an interval, chord, or scale.

INPUT a list of notes and a note
OUTPUT a list of lists of notes from the given scale that fit the given signature

-}

icsRecognizer :: [Note] -> Note -> String
icsRecognizer ns n
    | length ns == 0 = "not a pattern"
    | length sig == 1 = "DYAD: " ++ show n ++ " unison"
    | sig == sigInterval2m = "DYAD: " ++ show n ++ " minor second"
    | sig == sigInterval2M = "DYAD: " ++ show n ++ " major second"
    | sig == sigInterval3m = "DYAD: " ++ show n ++ " minor third"
    | sig == sigInterval3M = "DYAD: " ++ show n ++ " major third"
    | sig == sigInterval4p = "DYAD: " ++ show n ++ " perfect fourth"
    | sig == sigInterval4a = "DYAD: " ++ show n ++ " augmented fourth, aka " ++ show n ++ " tritone"
    | sig == sigInterval5p = "DYAD: " ++ show n ++ " perfect fifth"
    | sig == sigInterval6m = "DYAD: " ++ show n ++ " minor sixth"
    | sig == sigInterval6M = "DYAD: " ++ show n ++ " major sixth"
    | sig == sigInterval7m = "DYAD: " ++ show n ++ " minor seventh"
    | sig == sigInterval7M = "DYAD: " ++ show n ++ " major seventh"
    | sig == sigTriadd = "TRIAD: " ++ show n ++ " diminished"
    | sig == sigTriadm = "TRIAD: " ++ show n ++ " minor"
    | sig == sigTriadM = "TRIAD: " ++ show n ++ " major"
    | sig == sigTriada = "TRIAD: " ++ show n ++ " augmented"
    | sig == sig7ChordMM = "TETRAD: " ++ show n ++ " major seventh"
    | sig == sig7ChordMm = "TETRAD: " ++ show n ++ " seventh"
    | sig == sig7Chordmm = "TETRAD: " ++ show n ++ " minor major seventh"
    | sig == sig7ChordmM = "TETRAD: " ++ show n ++ " minor seventh"
    | sig == sig7Chorddm = "TETRAD: " ++ show n ++ " half-diminished, aka minor seventh b5"
    | sig == sig7Chordam = "TETRAD: " ++ show n ++ " augmented seventh"
    | sig == sig6ChordMM = "TETRAD: " ++ show n ++ " major sixth"
    | sig == sig6ChordmM = "TETRAD: " ++ show n ++ " minor sixth"
    | sig == sigScaleM = "SCALE: " ++ show n ++ " major, aka ionian, aka diatonic"
    | sig == sigScalemh = "SCALE: " ++ show n ++ " harmonic minor"
    | sig == sigScalemm = "SCALE: " ++ show n ++ " melodic minor"
    | sig == sigScaleMh = "SCALE: " ++ show n ++ " harmonic major"
    | sig == sigScaledh = "SCALE: " ++ show n ++ " double harmonic, aka byzantine, aka arabic, aka gypsy major"
    | sig == sigScalesd = "SCALE: " ++ show n ++ " symmetric diminished, aka diminished, aka octatonic, aka korsakovian, Pijper"
    | sig == sigScalec = "SCALE: " ++ show n ++ " chromatic"
    | sig == sigScalewt = "SCALE: " ++ show n ++ " whole-tone"
    | sig == sigScalepM = "SCALE: " ++ show n ++ " major pentatonic"
    | sig == sigScalepm = "SCALE: " ++ show n ++ " minor pentatonic"
    -- modes of the major scale
    | sig == sigMode 2 sigScaleM = "SCALE: " ++ show n ++ " dorian; the second mode of " ++ show (toNote $ (fromEnum n) - 2 ) ++ " major"
    | sig == sigMode 3 sigScaleM = "SCALE: " ++ show n ++ " phrygian; the third mode of " ++ show (toNote $ (fromEnum n) - 4 ) ++ " major"
    | sig == sigMode 4 sigScaleM = "SCALE: " ++ show n ++ " lydian; the fourth mode of " ++ show (toNote $ (fromEnum n) - 5 ) ++ " major"
    | sig == sigMode 5 sigScaleM = "SCALE: " ++ show n ++ " mixolydian; the fifth mode of " ++ show (toNote $ (fromEnum n) - 7 ) ++ " major"
    | sig == sigMode 6 sigScaleM = "SCALE: " ++ show n ++ " natural minor, aka aeolian; the sixth mode of " ++ show (toNote $ (fromEnum n) - 9 ) ++ " major"
    | sig == sigMode 7 sigScaleM = "SCALE: " ++ show n ++ " locrian; the seventh mode of " ++ show (toNote $ (fromEnum n) - 11 ) ++ " major"
    -- modes of the melodic minor scale
    | sig == sigMode 2 sigScalemm = "SCALE: " ++ show n ++ " dorian b2, aka phrygian 6, aka javanese, aka phrygidorian; the second mode of " ++ show (toNote $ (fromEnum n) - 2 ) ++ " melodic minor"
    | sig == sigMode 3 sigScalemm = "SCALE: " ++ show n ++ " lydian augmented, aka lydian #5; the third mode of " ++ show (toNote $ (fromEnum n) - 3 ) ++ " melodic minor"
    | sig == sigMode 4 sigScalemm = "SCALE: " ++ show n ++ " acoustic, aka overtone, aka lydian b7, aka lydian dominant, aka mixolydian #4, aka lydomyxian; the fourth mode of " ++ show (toNote $ (fromEnum n) - 5 ) ++ " melodic minor"
    | sig == sigMode 5 sigScalemm = "SCALE: " ++ show n ++ " aeolian dominant, aka melodic major, aka aeolian major, aka mixolydian b6, aka hindu, aka myxaeolian; the fifth mode of " ++ show (toNote $ (fromEnum n) - 7 ) ++ " melodic minor"
    | sig == sigMode 6 sigScalemm = "SCALE: " ++ show n ++ " half-diminished, aka locrian 2, aka aeolocrian; the sixth mode of " ++ show (toNote $ (fromEnum n) - 9 ) ++ " melodic minor"
    | sig == sigMode 7 sigScalemm = "SCALE: " ++ show n ++ " altered, aka altered dominant, aka super-locrian, aka locrian b4, aka Pomeroy, aka Ravel, aka diminished whole-tone, aka dominant whole-tone; the seventh mode of " ++ show (toNote $ (fromEnum n) - 11 ) ++ " melodic minor"
    -- modes of the harmonic minor scale
    | sig == sigMode 2 sigScalemh = "SCALE: " ++ show n ++ " locrian 6; the second mode of " ++ show (toNote $ (fromEnum n) - 2 ) ++ " harmonic minor"
    | sig == sigMode 3 sigScalemh = "SCALE: " ++ show n ++ " ionian #5; the third mode of " ++ show (toNote $ (fromEnum n) - 3 ) ++ " harmonic minor"
    | sig == sigMode 4 sigScalemh = "SCALE: " ++ show n ++ " spanish phrygian, aka romanian, aka ukrainian dorian, aka dorian #4; the fourth mode of " ++ show (toNote $ (fromEnum n) - 4 ) ++ " harmonic minor"
    | sig == sigMode 5 sigScalemh = "SCALE: " ++ show n ++ " phrygian dominant, aka phrygian major, aka altered phrygian, aka dominant b2 b6, aka freygish, aka mixolydian b9 b13 (Berklee); the fifth mode of " ++ show (toNote $ (fromEnum n) - 7 ) ++ " harmonic minor"
    | sig == sigMode 6 sigScalemh = "SCALE: " ++ show n ++ " lydian #2; the sixth mode of " ++ show (toNote $ (fromEnum n) - 8 ) ++ " harmonic minor"
    | sig == sigMode 7 sigScalemh = "SCALE: " ++ show n ++ " altered diminished; the seventh mode of " ++ show (toNote $ (fromEnum n) - 11 ) ++ " harmonic minor"
    -- modes of the harmonic major scale
    | sig == sigMode 2 sigScaleMh = "SCALE: " ++ show n ++ " dorian b5; the second mode of " ++ show (toNote $ (fromEnum n) - 2 ) ++ " harmonic major"
    | sig == sigMode 3 sigScaleMh = "SCALE: " ++ show n ++ " phrygian b4, aka altered 5; the third mode of " ++ show (toNote $ (fromEnum n) - 3 ) ++ " harmonic major"
    | sig == sigMode 4 sigScaleMh = "SCALE: " ++ show n ++ " lydian b3, aka lydian minor, aka lydian diminished, aka melodic minor #4; the fourth mode of " ++ show (toNote $ (fromEnum n) - 4 ) ++ " harmonic major"
    | sig == sigMode 5 sigScaleMh = "SCALE: " ++ show n ++ " mixolydian b2; the fifth mode of " ++ show (toNote $ (fromEnum n) - 7 ) ++ " harmonic major"
    | sig == sigMode 6 sigScaleMh = "SCALE: " ++ show n ++ " lydian augmented #2, aka aeolian b1; the sixth mode of " ++ show (toNote $ (fromEnum n) - 8 ) ++ " harmonic major"
    | sig == sigMode 7 sigScaleMh = "SCALE: " ++ show n ++ " locrian b7, aka locrian bb7, aka locrian diminished; the seventh mode of " ++ show (toNote $ (fromEnum n) - 11 ) ++ " harmonic major"
    -- modes of the double harmonic scale
    | sig == sigMode 2 sigScaledh = "SCALE: " ++ show n ++ " lydian #2 #6; the second mode of " ++ show (toNote $ (fromEnum n) - 2 ) ++ " double harmonic"
    | sig == sigMode 3 sigScaledh = "SCALE: " ++ show n ++ " ultraphrygian, aka phrygian b4 bb7; the third mode of " ++ show (toNote $ (fromEnum n) - 3 ) ++ " double harmonic"
    | sig == sigMode 4 sigScaledh = "SCALE: " ++ show n ++ " hungarian minor, aka double harmonic minor, aka harmonic minor #4, aka gypsy minor; the fourth mode of " ++ show (toNote $ (fromEnum n) - 4 ) ++ " double harmonic"
    | sig == sigMode 5 sigScaledh = "SCALE: " ++ show n ++ " oriental, aka locrian 3 6, aka mixolydian b2 b5; the fifth mode of " ++ show (toNote $ (fromEnum n) - 7 ) ++ " double harmonic"
    | sig == sigMode 6 sigScaledh = "SCALE: " ++ show n ++ " ionian augmented #2, aka ionian #2 #5; the sixth mode of " ++ show (toNote $ (fromEnum n) - 8 ) ++ " double harmonic"
    | sig == sigMode 7 sigScaledh = "SCALE: " ++ show n ++ " locrian bb3 bb7; the seventh mode of " ++ show (toNote $ (fromEnum n) - 11 ) ++ " double harmonic"
    -- mode of the symmetric diminished
    | sig == sigMode 2 sigScalesd = "SCALE: " ++ show n ++ " (half-whole-step) symmetric diminished; the second mode of " ++ show (toNote $ (fromEnum n) - 1) ++ " symmetric diminished"
    | otherwise = "unknown pattern"
    where
        sig = sort $ signatureByNotes $ nub $ n : ns

{- EXAMPLES -}

{-

> icsRecognizer [A, B, C, D, E, F, G] A
"SCALE: A natural minor, aka aeolian; the sixth mode of C major"

> icsRecognizer [D, Cs, B, As, Gs, Fs] E
"SCALE: E acoustic, aka overtone, aka lydian b7, aka lydian dominant, aka mixolydian #4, aka lydomyxian; the fourth mode of B melodic minor"

Still, for Part B of "This song will die", we are (indeed) in uncharted territory:

> icsRecognizer [E, F, Gs, As, B, C, D] E
"unknown pattern"

> icsRecognizer [E, F, Gs, As, B, C, Ds] E
"unknown pattern"

> icsRecognizer [E, F, Gs, As, B, Cs, D] E
"unknown pattern"

> icsRecognizer [E, F, Gs, As, B, Cs, Ds] E
"unknown pattern"

-}

{-----------------------------------------------------}
{- 1.4 -- APPLICATION: THE SEVENTH CHORDS OF A SCALE -}
{-----------------------------------------------------}

{-

INPUT a note (for the root) and a signature (for the type of scale)
OUTPUT a list of strings indicating all triads and all tetrads to be found within the induced scale

To do this, we combine applications 1.2 and 1.3.

-}

scalechordRecognizer :: Note -> [Int] -> [String]
scalechordRecognizer r scalesig = map (\ chord -> icsRecognizer chord (head chord)) $ concat $ map (\ chordsig -> scalechords (notesBySignature r scalesig) chordsig) [sigTriadM, sigTriadm, sigTriada, sigTriadd, sig7ChordMM, sig7ChordMm, sig7Chordmm, sig7ChordmM, sig7Chorddm, sig7Chordam]

{- EXAMPLES -}

{-

The chords of E minor pentatonic scale

> scalechordRecognizer E sigScalepm
["TRIAD: G major","TRIAD: E minor","TETRAD: E minor seventh"]

of E major scale

> scalechordRecognizer E sigScaleM
["TRIAD: E major","TRIAD: A major","TRIAD: B major","TRIAD: Fs minor","TRIAD: Gs minor","TRIAD: Cs minor","TRIAD: Ds diminished","TETRAD: E major seventh","TETRAD: A major seventh","TETRAD: B seventh","TETRAD: Fs minor seventh","TETRAD: Gs minor seventh","TETRAD: Cs minor seventh","TETRAD: Ds half-diminished, aka minor seventh b5"]

and of A double harmonic scale

> scalechordRecognizer A sigScaledh
["TRIAD: A major","TRIAD: As major","TRIAD: Cs major","TRIAD: As minor","TRIAD: Cs minor","TRIAD: D minor","TRIAD: A augmented","TRIAD: Cs augmented","TRIAD: F augmented","TRIAD: As diminished","TRIAD: D diminished","TETRAD: A major seventh","TETRAD: As major seventh","TETRAD: As seventh","TETRAD: As minor major seventh","TETRAD: D minor major seventh","TETRAD: As minor seventh","TETRAD: As half-diminished, aka minor seventh b5"]

-}

{--------------------------------------------}
{- 1.5 -- APPLICATION: A LITERATE SUGGESTER -}
{--------------------------------------------}

-- Now that we have a recognizer, we can also have a version of suggester (see application 1.1) where we get the names of the suggested scales or chords, rather than the note lists themselves.

scaleLSuggester :: [Note] -> Note -> [String]
scaleLSuggester ns r = filter (\ string -> string /= "unknown pattern") $ sort $ nub $ map (\ scale -> icsRecognizer scale r) $ scaleSuggester (sort $ nub $ ns) r

triadLSuggester :: [Note] -> Note -> [String]
triadLSuggester ns r = filter (\ string -> string /= "unknown pattern") $ sort $ nub $ map (\ triad -> icsRecognizer triad r) $ triadSuggester (sort $ nub $ ns) r

{--------------------------------------------------}
{- 2 -- AN INDUCTIVE TYPE FOR CHORD CONSTRUCTIONS -}
{--------------------------------------------------}

{-
So, one way to speak about chords is to view them as values of [Note], exactly as we did above with the four basic types of triads.
-}

type ChordL = [Note]

{-
Another way to capture all chords, which tracks their actual construction (as is sometimes the case in harmonic analysis), including single notes, intervals, triads, and polychords, is to use an inductive type, making use of the previously defined Note datatype. Here we restrict the chord datatype to up to "pentads"; not sure if we need more or less.
-}

data Chord = ChordBot | Single Note | Dyad Chord Chord | Triad Chord Chord Chord | Tetrad Chord Chord Chord Chord | Pentad Chord Chord Chord Chord Chord
    deriving (Eq, Ord, Show, Read)

{- EXAMPLES -}

e_minor_over_c = Dyad (Single C) (Triad (Single E) (Single G) (Single B))
a_over_c_major = Dyad (Triad (Single C) (Single E) (Single G)) (Single A)
c_major_extended = Dyad (Triad (Single C) (Single E) (Single G)) (ChordBot)

{-

> :t a_over_c_major
a_over_c_major :: Chord

> :t C
C :: Note

> :t Single C
Single C :: Chord

-}

{-
There is a bunch of functions to be defined on any inductive datatype. Note that the constructor "Single" takes values of "Note" as arguments, and it seems intuitive to view it as nullary in (some of?) the definitions that follow.
-}

-- The size and the height of a token (i.e., a chord).

sizeOfChordToken :: Chord -> Int
sizeOfChordToken ChordBot           = 0
sizeOfChordToken (Single _)         = 1
sizeOfChordToken (Dyad c d)         = 1 + (sizeOfChordToken c) + (sizeOfChordToken d)
sizeOfChordToken (Triad c d e)      = 1 + (sizeOfChordToken c) + (sizeOfChordToken d) + (sizeOfChordToken e)
sizeOfChordToken (Tetrad c d e f)   = 1 + (sizeOfChordToken c) + (sizeOfChordToken d) + (sizeOfChordToken e) + (sizeOfChordToken f)
sizeOfChordToken (Pentad c d e f g) = 1 + (sizeOfChordToken c) + (sizeOfChordToken d) + (sizeOfChordToken e) + (sizeOfChordToken f) + (sizeOfChordToken g)

heightOfChordToken :: Chord -> Int
heightOfChordToken ChordBot             = 0
heightOfChordToken (Single _)           = 1
heightOfChordToken (Dyad c d)           = 1 + maximum [heightOfChordToken c, heightOfChordToken d]
heightOfChordToken (Triad c d e)        = 1 + maximum [heightOfChordToken c, heightOfChordToken d, heightOfChordToken e]
heightOfChordToken (Tetrad c d e f)     = 1 + maximum [heightOfChordToken c, heightOfChordToken d, heightOfChordToken e, heightOfChordToken f]
heightOfChordToken (Pentad c d e f g)   = 1 + maximum [heightOfChordToken c, heightOfChordToken d, heightOfChordToken e, heightOfChordToken f, heightOfChordToken g]

-- The size of the chord, that is, its number of notes.

sizeOfChord :: Chord -> Int
sizeOfChord ChordBot            = 0
sizeOfChord (Single _)          = 1
sizeOfChord (Dyad c d)          = (sizeOfChord c) + (sizeOfChord d)
sizeOfChord (Triad c d e)       = (sizeOfChord c) + (sizeOfChord d) + (sizeOfChord e)
sizeOfChord (Tetrad c d e f)    = (sizeOfChord c) + (sizeOfChord d) + (sizeOfChord e) + (sizeOfChord f)
sizeOfChord (Pentad c d e f g)  = (sizeOfChord c) + (sizeOfChord d) + (sizeOfChord e) + (sizeOfChord f) + (sizeOfChord g)

-- The yield of a token, that is, its leaves, that is, its actual notes.

yieldOfChord :: Chord -> [Note]
yieldOfChord ChordBot           = []
yieldOfChord (Single n)         = [n]
yieldOfChord (Dyad c d)         = (yieldOfChord c) ++ (yieldOfChord d)
yieldOfChord (Triad c d e)      = (yieldOfChord c) ++ (yieldOfChord d) ++ (yieldOfChord e)
yieldOfChord (Tetrad c d e f)   = (yieldOfChord c) ++ (yieldOfChord d) ++ (yieldOfChord e) ++ (yieldOfChord f)
yieldOfChord (Pentad c d e f g) = (yieldOfChord c) ++ (yieldOfChord d) ++ (yieldOfChord e) ++ (yieldOfChord f) ++ (yieldOfChord g)

{- EXAMPLES

> sizeOfChordToken e_minor_over_c
6

> heightOfChordToken e_minor_over_c
3

> sizeOfChord e_minor_over_c
4

> yieldOfChord e_minor_over_c
[C,E,G,B]

> yieldOfChord a_over_c_major
[C,E,G,A]

> sort (yieldOfChord a_over_c_major)
[A,C,E,G]

> :t Triad (Single C) (Single E) (Single G)
(Triad (Single C) (Single E) (Single G)) :: Chord

> :t yieldOfChord ((Triad (Single C) (Single E) (Single G)))
yieldOfChord ((Triad (Single C) (Single E) (Single G))) :: [Note]

The size of a chord is just the length of its yield.

> (sizeOfChord (Triad (Single C) (Single E) (Single G))) == length (yieldOfChord (Triad (Single C) (Single E) (Single G)))
True

-}

-- Define the head constructor C of a token C a1 ... ar as the token C * ... *.

headToken :: Chord -> Chord
headToken ChordBot = ChordBot
headToken (Single n) = Single n
headToken (Dyad c d) = Dyad ChordBot ChordBot
headToken (Triad c d e) = Triad ChordBot ChordBot ChordBot
headToken (Tetrad c d e f) = Tetrad ChordBot ChordBot ChordBot ChordBot
headToken (Pentad c d e f g) = Pentad ChordBot ChordBot ChordBot ChordBot ChordBot

-- Define the arity of a token; more general, and easier than declaring the arity of a constructor. In particular, what is here defined is the arity of the head constructor of a token.

arity :: Chord -> Int
arity ChordBot              = 0
arity (Single _)            = 0 -- here Single is treated as a nullary constructor!
arity (Dyad _ _)            = 2
arity (Triad _ _ _)         = 3
arity (Tetrad _ _ _ _)      = 4
arity (Pentad _ _ _ _ _)    = 5

-- Define the list of component tokens of a given token.

tokenComponents :: Chord -> [Chord]
tokenComponents ChordBot             = []
tokenComponents (Single _)           = []
tokenComponents (Dyad c d)           = [c, d]
tokenComponents (Triad c d e)        = [c, d, e]
tokenComponents (Tetrad c d e f)     = [c, d, e, f]
tokenComponents (Pentad c d e f g)   = [c, d, e, f, g]

-- Define the list of all subtokens of a given token.

subtokens :: Chord -> [Chord]
subtokens ChordBot              = [ChordBot]
subtokens (Single n)            = [Single n]
subtokens (Dyad c d)            = [Dyad c d] ++ (subtokens c) ++ (subtokens d)
subtokens (Triad c d e)         = [Triad c d e] ++ (subtokens c) ++ (subtokens d) ++ (subtokens e)
subtokens (Tetrad c d e f)      = [Tetrad c d e f] ++ (subtokens c) ++ (subtokens d) ++ (subtokens e) ++ (subtokens f)
subtokens (Pentad c d e f g)    = [Pentad c d e f g] ++ (subtokens c) ++ (subtokens d) ++ (subtokens e) ++ (subtokens f) ++ (subtokens g)


{- EXAMPLES

> componentToken a_over_c_major 1
Just (Triad (Single C) (Single E) (Single G))

> componentToken a_over_c_major 2
Just (Single A)

> componentToken a_over_c_major 3
Nothing

> tokenComponents a_over_c_major
[Triad (Single C) (Single E) (Single G),Single A]

> tokenComponents e_minor_over_c
[Single C,Triad (Single E) (Single G) (Single B)]

> subtokens e_minor_over_c
[Dyad (Single C) (Triad (Single E) (Single G) (Single B)),Single C,Triad (Single E) (Single G) (Single B),Single E,Single G,Single B]

-}

-- Define the usual consistency predicates.

consistent :: Chord -> Chord -> Bool
consistent _ ChordBot   = True
consistent ChordBot _   = True
consistent c d          = headToken c == headToken d && consistentLift (tokenComponents c) (tokenComponents d)

consistentLift :: [Chord] -> [Chord] -> Bool
consistentLift [] []           = True
consistentLift (c:cs) (d:ds)   = consistent c d && consistentLift cs ds
consistentLift _ _             = False

consistentL :: [Chord] -> Bool
consistentL []       = True
consistentL [_]      = True
consistentL (c:d:cs) = consistent c d && consistentL (c:cs)

{- EXAMPLES

> c_major_extended `consistent` a_over_c_major
True

> c_major_extended `consistent` e_minor_over_c
False

> consistentL [c_major_extended, a_over_c_major, e_minor_over_c]
False

> consistentL [c_major_extended, a_over_c_major]
True

> consistentL [Single A, ChordBot, Single B]
False

-}

-- An auxiliary function for the notion of sufficiency: a neighborhood U is sufficient for the constructor C of arity r on the arguments U1, ..., Ur, when for each i = 1, ..., r and each ai in Ui there exists an a in U with head constructor C and i-th component token ai.

sufficient :: [Chord] -> Chord -> [[Chord]] -> Bool -- arg1: U, arg2: C, arg3: U1, 6..., Ur
sufficient u ctr us
    | not (consistentL u) = error "sufficient: inconsistent list"
    | ctr /= headToken ctr = error "sufficient: not a constructor"
    | length us /= r = error "sufficient: too few or too many arguments"
    | otherwise = all (\ i -> all (\ b -> any (\ a -> headToken a == ctr && ((tokenComponents a) !! (i - 1)) == b) u) (us !!(i-1))) indices
    where
        r = arity ctr
        indices = [1..r]

-- An auxiliary function for the constructor application.

ctrapply :: Chord -> [[Chord]] -> [Chord]
ctrapply ctr us
    | ctr /= ctrhead                                                    = error "ctrapply: not a constructor"
--     | any (\ u -> u == []) us                                           = error "ctrapply: empty argument" -- if you include this line the pathform below crashes; in any case, you would need the line in the definition of entails, if you were to use ctrapply there
    | any (\ u -> not (consistentL u)) us                               = error "ctrapply: inconsistent argument"
    | length us /= arity ctr                                            = error "ctrapply: too few or too many arguments"
    | ctrhead == ChordBot                                               = [ChordBot]
    | ctrhead == Single A                                               = [Single A]
    | ctrhead == Single As                                              = [Single As]
    | ctrhead == Single B                                               = [Single B]
    | ctrhead == Single C                                               = [Single C]
    | ctrhead == Single Cs                                              = [Single Cs]
    | ctrhead == Single D                                               = [Single D]
    | ctrhead == Single Ds                                              = [Single Ds]
    | ctrhead == Single E                                               = [Single E]
    | ctrhead == Single F                                               = [Single F]
    | ctrhead == Single Fs                                              = [Single Fs]
    | ctrhead == Single G                                               = [Single G]
    | ctrhead == Single Gs                                              = [Single Gs]
    | ctrhead == Dyad ChordBot ChordBot                                 = [Dyad a b | a <- (us !! 0), b <- (us !! 1)]
    | ctrhead == Triad ChordBot ChordBot ChordBot                       = [Triad a b c | a <- (us !! 0), b <- (us !! 1), c <- (us !! 2)]
    | ctrhead == Tetrad ChordBot ChordBot ChordBot ChordBot             = [Tetrad a b c d | a <- (us !! 0), b <- (us !! 1), c <- (us !! 2), d <- (us !! 3)]
    | ctrhead == Pentad ChordBot ChordBot ChordBot ChordBot ChordBot    = [Pentad a b c d e | a <- (us !! 0), b <- (us !! 1), c <- (us !! 2), d <- (us !! 3), e <- (us !! 4)]
    where
        ctrhead = headToken ctr

-- The following strips a neighborhood of its bottom tokens.

stripBot :: [Chord] -> [Chord]
stripBot u = filter (\ a -> a /= ChordBot) u

-- Define the predicate of entailment; interesting that it works without ctrapply or stripBot -- this needs testing.

entails :: [Chord] -> Chord -> Bool
entails u a
    | not (consistentL u)       = error "inconsistent list"
    | otherwise                 = case a of
                                        ChordBot         -> True
                                        Single n         -> sufficient u (headToken a) [[Single n]]
                                        Dyad b c         -> sufficient u (headToken a) [[b], [c]]
                                        Triad b c d      -> sufficient u (headToken a) [[b], [c], [d]]
                                        Tetrad b c d e   -> sufficient u (headToken a) [[b], [c], [d], [e]]
                                        Pentad b c d e f -> sufficient u (headToken a) [[b], [c], [d], [e], [f]]

-- Finally, define the lift of entails between lists.

entailsL :: [Chord] -> [Chord] -> Bool
entailsL u v = all (\ b -> u `entails` b) v

{- EXAMPLES

> entails [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] (Dyad ChordBot ChordBot)
True

> entails [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] (Dyad e_minor_over_c ChordBot)
True

> entails [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] (Dyad e_minor_over_c (Dyad (Triad (Single C) (Single E) (Single G)) (Single A)))
True

> entails [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] (Dyad e_minor_over_c (Dyad (Triad (Single C) (Single E) (Single G)) ChordBot))
True

> entails [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] (Dyad e_minor_over_c (Dyad (Triad (Single C) (Single E) (Single G)) (Single B)))
False

> [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] `entails` (Dyad e_minor_over_c (Dyad (Triad (Single C) (Single E) (Single G)) (Single B)))
False

> [ChordBot, Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] `entails` (Dyad e_minor_over_c ChordBot)
True

> [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] `entails` (Triad e_minor_over_c ChordBot ChordBot)
False

> [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] `entailsL` [ChordBot,Triad e_minor_over_c ChordBot ChordBot]
False

> [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] `entailsL` [ChordBot,Dyad e_minor_over_c ChordBot]
True

-}

-- Now to some normal forms of neighborhoods. The eigentoken (or supremum) of a neighborhood is defined with the help of an auxiliary function computing suprema of two tokens.

sup :: Chord -> Chord -> Chord
sup ChordBot a = a
sup a ChordBot = a
sup (Single A) (Single A) = (Single A)
sup (Single As) (Single As) = (Single As)
sup (Single B) (Single B) = (Single B)
sup (Single C) (Single C) = (Single C)
sup (Single Cs) (Single Cs) = (Single Cs)
sup (Single D) (Single D) = (Single D)
sup (Single Ds) (Single Ds) = (Single Ds)
sup (Single E) (Single E) = (Single E)
sup (Single F) (Single F) = (Single F)
sup (Single Fs) (Single Fs) = (Single Fs)
sup (Single G) (Single G) = (Single G)
sup (Single Gs) (Single Gs) = (Single Gs)
sup (Dyad a b) (Dyad a' b') = Dyad (sup a a') (sup b b')
sup (Triad a b c) (Triad a' b' c') = Triad (sup a a') (sup b b') (sup c c')
sup (Tetrad a b c d) (Tetrad a' b' c' d') = Tetrad (sup a a') (sup b b') (sup c c') (sup d d')
sup (Pentad a b c d e) (Pentad a' b' c' d' e') = Pentad (sup a a') (sup b b') (sup c c') (sup d d') (sup e e')
sup _ _ = error "sup: inconsistent tokens"

supL :: [Chord] -> Chord
supL [] = ChordBot
supL [a] = a
supL (a:as) = sup a (supL as)

-- The path form of a neighborhood is defined with the help of an auxiliary function computing the path form of a token.

paths :: Chord -> [Chord]
paths ChordBot            = [ChordBot]
paths (Single A)          = [Single A]
paths (Single As)         = [Single As]
paths (Single B)          = [Single B]
paths (Single C)          = [Single C]
paths (Single Cs)         = [Single Cs]
paths (Single D)          = [Single D]
paths (Single Ds)         = [Single Ds]
paths (Single E)          = [Single E]
paths (Single F)          = [Single F]
paths (Single Fs)         = [Single Fs]
paths (Single G)          = [Single G]
paths (Single Gs)         = [Single Gs]
paths (Dyad a b)          = nub $ (ctrapply (Dyad ChordBot ChordBot) [stripBot $ paths a, [ChordBot]]) ++ (ctrapply (Dyad ChordBot ChordBot) [[ChordBot], stripBot $ paths b])
paths (Triad a b c)       = nub $ (ctrapply (Triad ChordBot ChordBot ChordBot) [stripBot $ paths a, [ChordBot], [ChordBot]]) ++ (ctrapply (Triad ChordBot ChordBot ChordBot) [[ChordBot], stripBot $ paths b, [ChordBot]]) ++ (ctrapply (Triad ChordBot ChordBot ChordBot)  [[ChordBot], [ChordBot], stripBot $ paths c])
paths (Tetrad a b c d)    = nub $ (ctrapply (Tetrad ChordBot ChordBot ChordBot ChordBot) [stripBot $ paths a, [ChordBot], [ChordBot], [ChordBot]]) ++ (ctrapply (Tetrad ChordBot ChordBot ChordBot ChordBot) [[ChordBot], stripBot $ paths b, [ChordBot], [ChordBot]]) ++ (ctrapply (Tetrad ChordBot ChordBot ChordBot ChordBot)  [[ChordBot], [ChordBot], stripBot $ paths c, [ChordBot]]) ++ (ctrapply (Tetrad ChordBot ChordBot ChordBot ChordBot) [[ChordBot], [ChordBot], [ChordBot], stripBot $ paths d])
paths (Pentad a b c d e)  = nub $ (ctrapply (Pentad ChordBot ChordBot ChordBot ChordBot ChordBot) [stripBot $ paths a, [ChordBot], [ChordBot], [ChordBot], [ChordBot]]) ++ (ctrapply (Pentad ChordBot ChordBot ChordBot ChordBot ChordBot) [[ChordBot], stripBot $ paths b, [ChordBot], [ChordBot], [ChordBot]]) ++ (ctrapply (Pentad ChordBot ChordBot ChordBot ChordBot ChordBot)  [[ChordBot], [ChordBot], stripBot $ paths c, [ChordBot], [ChordBot]]) ++ (ctrapply (Pentad ChordBot ChordBot ChordBot ChordBot ChordBot) [[ChordBot], [ChordBot], [ChordBot], stripBot $ paths d, [ChordBot]]) ++ (ctrapply (Pentad ChordBot ChordBot ChordBot ChordBot ChordBot) [[ChordBot], [ChordBot], [ChordBot], [ChordBot], stripBot $ paths e])

pathsL :: [Chord] -> [Chord]
pathsL u = paths (supL u)

{- EXAMPLES

> sup (Dyad (Single Cs) (ChordBot)) (Dyad (ChordBot) (Single D))
Dyad (Single Cs) (Single D)

> sup (Dyad (Single Cs) (ChordBot)) (Single D)
*** Exception: sup: inconsistent tokens

> supL [Triad (Single C) ChordBot ChordBot, Triad ChordBot (Single E) ChordBot, Triad ChordBot ChordBot (Single G), ChordBot, Triad (Single C) (Single E) ChordBot]
Triad (Single C) (Single E) (Single G)

> supL []
ChordBot

> supL [Triad (Single C) ChordBot ChordBot, Triad ChordBot (Single E) ChordBot, Triad ChordBot ChordBot (Single G), ChordBot, Triad (Single F) ChordBot ChordBot]
Triad *** Exception: sup: inconsistent tokens

> paths ChordBot
[ChordBot]

> paths (Single A)
[Single A]

> paths (Dyad ChordBot (Single A))
[Dyad ChordBot (Single A)]

> paths (Pentad (Single E) (Single G) (Single B) (Single D) (Pentad (Single E) (Single G) (Single B) (Single D) ChordBot))
[Pentad (Single E) ChordBot ChordBot ChordBot ChordBot,Pentad ChordBot (Single G) ChordBot ChordBot ChordBot,Pentad ChordBot ChordBot (Single B) ChordBot ChordBot,Pentad ChordBot ChordBot ChordBot (Single D) ChordBot,Pentad ChordBot ChordBot ChordBot ChordBot (Pentad (Single E) ChordBot ChordBot ChordBot ChordBot),Pentad ChordBot ChordBot ChordBot ChordBot (Pentad ChordBot (Single G) ChordBot ChordBot ChordBot),Pentad ChordBot ChordBot ChordBot ChordBot (Pentad ChordBot ChordBot (Single B) ChordBot ChordBot),Pentad ChordBot ChordBot ChordBot ChordBot (Pentad ChordBot ChordBot ChordBot (Single D) ChordBot)]

> paths (Dyad (Triad (Single E) (Single G) (Single B)) (Pentad (Single E) (Single G) (Single B) (Single D) ChordBot))
[Dyad (Triad (Single E) ChordBot ChordBot) ChordBot,Dyad (Triad ChordBot (Single G) ChordBot) ChordBot,Dyad (Triad ChordBot ChordBot (Single B)) ChordBot,Dyad ChordBot (Pentad (Single E) ChordBot ChordBot ChordBot ChordBot),Dyad ChordBot (Pentad ChordBot (Single G) ChordBot ChordBot ChordBot),Dyad ChordBot (Pentad ChordBot ChordBot (Single B) ChordBot ChordBot),Dyad ChordBot (Pentad ChordBot ChordBot ChordBot (Single D) ChordBot)]

-}

-- it's high time we had some pretty-printing...

ppn :: Note -> String
ppn n = show n

ppnL :: [Note] -> [String]
ppnL ns = map ppn ns

ppc :: Chord -> String
ppc ChordBot           = "*"
ppc (Single n)         = ppn n
ppc (Dyad c d)         = "(2" ++ (ppc c) ++ (ppc d) ++ ")"
ppc (Triad c d e)      = "(3" ++ (ppc c) ++ (ppc d) ++ (ppc e) ++ ")"
ppc (Tetrad c d e f)   = "(4" ++ (ppc c) ++ (ppc d) ++ (ppc e) ++ (ppc f) ++ ")"
ppc (Pentad c d e f g) = "(5" ++ (ppc c) ++ (ppc d) ++ (ppc e) ++ (ppc f) ++ (ppc g) ++ ")"

ppcL :: [Chord] -> [String]
ppcL as = map ppc as

{- EXAMPLES

> ppcL $ paths ChordBot
["*"]

> ppcL $ paths (Single A)
["A"]

> ppcL $ paths (Dyad ChordBot (Single A))
["(2*A)"]

> ppcL $ paths (Pentad (Single E) (Single G) (Single B) (Single D) (Pentad (Single E) (Single G) (Single B) (Single D) ChordBot))
["(5E****)","(5*G***)","(5**B**)","(5***D*)","(5****(5E****))","(5****(5*G***))","(5****(5**B**))","(5****(5***D*))"]

> ppcL $ paths (Dyad (Triad (Single E) (Single G) (Single B)) (Pentad (Single E) (Single G) (Single B) (Single D) ChordBot))
["(2(3E**)*)","(2(3*G*)*)","(2(3**B)*)","(2*(5E****))","(2*(5*G***))","(2*(5**B**))","(2*(5***D*))"]

> ppc a_over_c_major
"(2(3CEG)A)"

> ppc e_minor_over_c
"(2C(3EGB))"

-}

{---------------------}
{- 3 -- ABOUT RHYTHM -}
{---------------------}

-- We may view time signatures like 4/4, 3/4, et cetera, as pairs of integers.

type TimeSig = (Int,Int) -- well, the integers are supposed to be positive

-- Given a time signature and a number of bars, give all rhythmic patterns for the whole span of bars.

rhythmPatterns :: (Int,Int) -> Int -> [[Bool]]
rhythmPatterns tsig b = [map (\ x -> elem x (numPatterns !! i)) allBeats | i <- [0..(len2-1)]] {-[map (\ x -> elem x (numPatterns !! i)) allBeats | i <- [0..len]]-} {-[ [elem x (numPatterns !! i) | x <- allBeats] | i <- [0..len]]-}
    where
        len = length allBeats
        numPatterns = concat [choose i allBeats | i <- [0..len]]
        allBeats = [1..(b * (fst tsig))]
        len2 = length numPatterns

-- The following yields all rhythm patterns that stress exactly m many beats of all

rhythmPatternsWith :: (Int, Int) -> Int -> Int -> [[Bool]]
rhythmPatternsWith tsig b m = filter ([False | i <- [1..((b*(fst tsig)) - m)]] `isSubsequenceOf`) $ filter ([True | i <- [1..m]] `isSubsequenceOf` ) $ rpats
    where rpats = rhythmPatterns tsig b

{- EXAMPLES -}

{-

Say we want to practice on accentuating 2 out of 4 beats, i.e., out of one bar of a typical 4/4 song.

> rhythmPatternsWith (4,4) 1 2
[[True,True,False,False],[True,False,True,False],[True,False,False,True],[False,True,True,False],[False,True,False,True],[False,False,True,True]]

Here are all ways to accentuate three out of eight eighths.

> rhythmPatternsWith (8,8) 1 3
[[True,True,True,False,False,False,False,False],[True,True,False,True,False,False,False,False],[True,True,False,False,True,False,False,False],[True,True,False,False,False,True,False,False],[True,True,False,False,False,False,Tr
ue,False],[True,True,False,False,False,False,False,True],[True,False,True,True,False,False,False,False],[True,False,True,False,True,False,False,False],[True,False,True,False,False,True,False,False],[True,False,True,False,False
,False,True,False],[True,False,True,False,False,False,False,True],[True,False,False,True,True,False,False,False],[True,False,False,True,False,True,False,False],[True,False,False,True,False,False,True,False],[True,False,False,T
rue,False,False,False,True],[True,False,False,False,True,True,False,False],[True,False,False,False,True,False,True,False],[True,False,False,False,True,False,False,True],[True,False,False,False,False,True,True,False],[True,Fals
e,False,False,False,True,False,True],[True,False,False,False,False,False,True,True],[False,True,True,True,False,False,False,False],[False,True,True,False,True,False,False,False],[False,True,True,False,False,True,False,False],[
False,True,True,False,False,False,True,False],[False,True,True,False,False,False,False,True],[False,True,False,True,True,False,False,False],[False,True,False,True,False,True,False,False],[False,True,False,True,False,False,True
,False],[False,True,False,True,False,False,False,True],[False,True,False,False,True,True,False,False],[False,True,False,False,True,False,True,False],[False,True,False,False,True,False,False,True],[False,True,False,False,False,
True,True,False],[False,True,False,False,False,True,False,True],[False,True,False,False,False,False,True,True],[False,False,True,True,True,False,False,False],[False,False,True,True,False,True,False,False],[False,False,True,Tru
e,False,False,True,False],[False,False,True,True,False,False,False,True],[False,False,True,False,True,True,False,False],[False,False,True,False,True,False,True,False],[False,False,True,False,True,False,False,True],[False,False
,True,False,False,True,True,False],[False,False,True,False,False,True,False,True],[False,False,True,False,False,False,True,True],[False,False,False,True,True,True,False,False],[False,False,False,True,True,False,True,False],[Fa
lse,False,False,True,True,False,False,True],[False,False,False,True,False,True,True,False],[False,False,False,True,False,True,False,True],[False,False,False,True,False,False,True,True],[False,False,False,False,True,True,True,F
alse],[False,False,False,False,True,True,False,True],[False,False,False,False,True,False,True,True],[False,False,False,False,False,True,True,True]]
*Main>

Lacking a gui, it would be nice to have a pretty-print of this function too---although the complexity is, of course, what it is---ma non adesso.

-}

{-------------------------------------}
{- 4 -- TYMOCZKO CHORD CHANGE SPACES -}
{-------------------------------------}

{-

The following draws from [Tymoczko-2006, Tymoczko-2011-book, Tymoczko-2012], and quite substantially from [Hall-Tymoczko-2012]. We'll use n-dimensional Moebius-strip-like spaces, called "orbifolds" in differential geometry, to model harmonies of n-note chord changes, or better, of n-note voice leadings. Let's see how far we can get without using something like the "Data.Modular" module. A basic correspondence to guide the intuition is given by

music - math
chord - point
voice leading - vector
scale - metric

The point of departure is to understand that "n-chords" are simply n-tuples of notes, and chord changes are differences (say, in halfsteps) between the corresponding notes in each voice; to define types to talk about these things wouldn't be a big deal. The interest lies in the notion of distance between two such changes; it is the definition of a distance that would turn each such type into a metric space, but there are good musical reasons to not want to restrict to any particular metric. Instead, we're going to define a partial order in these types, called "submajorization". I think here of the resulting structures as "Tymoczko chord change spaces" or just "Tymoczko spaces". It would be interesting to have a definition of this partial order by induction on n---we'll see.

-}

-- First define the submajorization order in some generality. The predicate "c submaj d" is to be read as "c is submajorized by d", so we have a kind of "less than or equal to".

submaj :: [Int] -> [Int] -> Bool
submaj xs ys
  | length ys == len = foldl (&&) True [(sum $ take i sxs) <= (sum $ take i sys) | i <- [1..len]]
  | otherwise = error "submaj: inputs have unequal length"
  where
    len = length xs
    sxs = sortDesc xs
    sys = sortDesc ys

-- Given two n-voicings v and w, define their displacement set. Note that for this purpose we need zip not with any (symmetric) distance function, but with the minimal change function, since we're interested in voice leadings, that is, in changes between voicings: we have to take into account that a single voice may move higher or lower.

displacement :: [Note] -> [Note] -> [Int]
displacement ns ms
  | length ms == length ns = zipWith minChange ns ms
  | otherwise = error "displacement: inputs have unequal lengths"

{- EXAMPLES -}

{-

> displacement [C, D, G] [C, F, A]
[0,3,2]

> displacement [C, D, G] [Cs, D, G]
[1,0,0]

> displacement [C, E, G] [Cs, E, Fs]
[1,0,-1]

> submaj (displacement [C, D, G] [Cs, D, G]) (displacement [C, D, G] [C, F, A])
True

The following adapts the example from Figure 4 in [Hall-Tymoczko-2012].

> submaj (displacement [C, Ds] [D, F]) (displacement [C, Ds] [F,D])
True

And this one adapts the example from [Hall-Tymoczko-2012, pp. 280, 282], concerning which chord divides the octave more evenly: C major triad or C suspended fourth triad (which is not supported yet above!)? According to our implementation so far we get

> submaj (displacement [C, E, G] [C, E, Gs]) (displacement [C, F, G] [C, E, Gs])
False

and

> submaj (displacement [C, F, G] [C, E, Gs]) (displacement [C, E, G] [C, E, Gs])
True

which is contrary to traditional musical intuition. Obviously, we need the notion of "T-closeness" from the above reference... Now, this will be tricky, since this notion is defined by existential quantification.

-}

{----------------------}
{- 4.1 -- T-CLOSENESS -}
{----------------------}

{-

We follow [Hall-Tymoczko-2012] a bit more closely, in particular, from section 2.1 on.

-}

-- Define the "sigma j" of a list, that is, the sum of the j largest elements of a list (the authors say "multiset").

largestElSum :: Int -> [Int] -> Int
largestElSum j ns = sum $ take j (sortDesc ns)

-- Define the "sigma j ball" of a list [Hall-Tymoczko-2012, Definition 5] as a predicate. Input a list (for the vector), a j, and a list (for the multiset), and output "yes" if the vector is in the sigma j ball of the multiset.

inBall :: [Int] -> Int -> [Int] -> Bool
inBall vs j ns = largestElSum j (map abs vs) <= largestElSum j ns

-- With the help of inBall, define the inner and outer submajorization balls. The inner one is the intersection of all sigma j balls, while the outer one is their union.

inInnerBall :: [Int] -> [Int] -> Bool
inInnerBall vs ns = all (\ j -> inBall vs j ns) [1..(length vs)]

inOuterBall :: [Int] -> [Int] -> Bool
inOuterBall vs ns = any (\ j -> inBall vs j ns) [1..(length vs)]

{- EXAMPLES -}

{-

> inBall [1,-2,3] 1 [2,2,2]
False

> inBall [1,-2,3] 2 [2,2,2]
False

> inBall [1,-2,3] 3 [2,2,2]
True

> inInnerBall [1,-2,3] [2,2,2]
False

> inOuterBall [1,-2,3] [2,2,2]
True

> inInnerBall [1,-1,1] [2,2,2]
True

> inOuterBall [1,-1,1] [2,2,2]
True

-}

-- Now to the concept of T-closeness. Given a change X = {x_1, ..., x_n}, say "absolute change" and write |X| for the change {|x_1|, ..., |x_n|}; also, write X-x for the change {x_1 - x, ..., x_n - x}. A straightforward interpretation of Definition 6 of Hall & Tymoczko, leads us to say that an n-change W is T-smaller than an n-change V if and only if for all 1-changes x there exist 1-changes y such that the absolute change |W - y| is submajorized by the absolute change |V - x|. The immediate questions are: is the carrier of x exhaustible? is the carrier of y searchable? (in the sense of [Escardo-2008]) Well, duh: in our case they can both taken to be finite, so apparently we don't even need the authors' algorithm in Appendix A.2.

tsmaller :: [Int] -> [Int] -> Bool
tsmaller ws vs = all (\ x -> (any (\ y -> submaj (map abs $ map (\ w -> w - y) ws) (map abs $ map (\ v -> v - x) vs)) xyrange)) xyrange
  where
    xyrange = [1..12]

{- EXAMPLES -}

{-

> tsmaller (displacement [C, F, G] [C, E, Gs]) (displacement [C, E, G] [C, E, Gs])
False

-}

-- Nevertheless, it doesn't hurt in principle to have the full generality. This we would need, had we introduced the type Note as a continuous type.

-- Define the middle interval of a list.

middleInterval :: (Fractional t, Ord t) => [t] -> [t]
middleInterval xs = [minimum mset, maximum mset]
  where
    mset = map (\ (m, n) -> (m + n)/2) $ zip supxs sdownxs
    supxs = sort xs
    sdownxs = sortDesc xs

-- Define the set of averages of a given list (the set S of [Hall-Tymoczko-2012, Theorem2]).

averages :: (Eq a, Fractional a) => [a] -> [a]
averages xs = nub $ map (\ (m, n) -> (m + n)/2) [(x, y) | x <- xs, y <- xs]

{------------------------------}
{- 5 -- CONCERNING THE GUITAR -}
{------------------------------}

{-
Watching this guy here, https://www.youtube.com/watch?v=b9pYEjZ4l48, it seemed reasonable to start implementing some of the quirks that are particular to the guitar.
-}

-- Start by introducing two basic datatypes, one for the strings and one for the frets.

data Guitarstring = StringOne | StringTwo | StringThree | StringFour | StringFive | StringSix
  deriving (Eq, Enum, Ord, Show, Read, Bounded)

data Fret = FretZero | FretOne | FretTwo | FretThree | FretFour | FretFive | FretSix | FretSeven | FretEight | FretNine | FretTen | FretEleven | FretTwelve | FretThirteen | FretFourteen | FretFifteen | FretSixteen | FretSeventeen | FretEighteen | FretNineteen| FretTwenty| FretTwentyone| FretTwentytwo
  deriving (Eq, Enum, Ord, Show, Read, Bounded)

-- Then a couple of the common tunings.

tuningStandard :: Guitarstring -> Note
tuningStandard string = case string of
  StringOne -> E
  StringTwo -> B
  StringThree -> G
  StringFour -> D
  StringFive -> A
  StringSix -> E

tuningDropD :: Guitarstring -> Note
tuningDropD string = case string of
  StringOne -> E
  StringTwo -> B
  StringThree -> G
  StringFour -> D
  StringFive -> A
  StringSix -> D

tuningOpenD :: Guitarstring -> Note
tuningOpenD string = case string of
  StringOne -> D
  StringTwo -> A
  StringThree -> Fs
  StringFour -> D
  StringFive -> A
  StringSix -> D

tuningDadgad :: Guitarstring -> Note
tuningDadgad string = case string of
  StringOne -> D
  StringTwo -> A
  StringThree -> G
  StringFour -> D
  StringFive -> A
  StringSix -> D

tuningNST :: Guitarstring -> Note
tuningNST string = case string of
  StringOne -> G
  StringTwo -> E
  StringThree -> A
  StringFour -> D
  StringFive -> G
  StringSix -> C

-- Just a grain of sugar.

type Tuning = Guitarstring -> Note

-- And now, the fretboard; given a tuning, we allot notes to the pairs of strings and frets.

fretboard :: Tuning -> Guitarstring -> Fret -> Note
fretboard tuning string fret = halfsteps (tuning string) (fromEnum fret)

{-----------------------------------------}
{- 5.1 -- APPLICATION: FRETBOARD QUIZZES -}
{-----------------------------------------}

{-
We can now implement the quizzes that the youtube guy from above suggested. For this, we will need to use the IO monad. We will also need the Random module. See https://wiki.haskell.org/Introduction_to_IO, https://wiki.haskell.org/IO_inside and  https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/randoms.
-}

-- To be able to generate random strings, frets, or notes for the needs of the quiz, we need to make the respective types into instances of the class Random.

instance Random Guitarstring where
  randomR (string,string') generator = case randomR (fromEnum string, fromEnum string') generator of
    (stringNumber, generator') -> (toEnum stringNumber, generator')
  random = randomR (minBound,maxBound)

instance Random Fret where
  randomR (fret,fret') generator = case randomR (fromEnum fret, fromEnum fret') generator of
    (fretNumber, generator') -> (toEnum fretNumber, generator')
  random = randomR (minBound,maxBound)

instance Random Note where
  randomR (note,note') generator = case randomR (fromEnum note, fromEnum note') generator of
    (noteNumber, generator') -> (toEnum noteNumber, generator')
  random = randomR (minBound,maxBound)

-- Now we can define the quizzes as follows.

fretboardQuizOneStandard :: IO ()
fretboardQuizOneStandard = do
  g1 <- newStdGen
  g2 <- newStdGen
  let fret = head (randoms g1 :: [Fret])
  let string = head (randoms g2 :: [Guitarstring])
  let answer = fretboard tuningStandard string fret
  putStrLn ("What is the note on fret " ++ show (fromEnum fret) ++ " of string " ++ show ((fromEnum string) + 1) ++ " in standard tuning?")
  putStrLn " nomenclature: A, As, B, C, Cs, D, ..."
  guess <- getLine
  if guess == show answer then putStr " Correct!" else putStr (" Wrong! The note is actually " ++ (show answer) ++ ".")

fretboardQuizTwoStandard :: IO ()
fretboardQuizTwoStandard = do
  g1 <- newStdGen
  g2 <- newStdGen
  let note = head (randoms g1 :: [Note])
  let string = head (randoms g2 :: [Guitarstring])
  putStrLn ("Where is the note " ++ (show note) ++ " on string " ++ show ((fromEnum string) + 1) ++ " in standard tuning?")
  guess <- readLn --getLine
  if fretboard tuningStandard string (toEnum guess) == note then putStr " Correct!" else putStr " Wrong." -- Needs some more work to also provide the user with the correct answer here.
