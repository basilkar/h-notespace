{--------------------------------------}
{- 1 -- AN ENUMERATION TYPE FOR NOTES -}
{--------------------------------------}

module Note where

import Data.List -- for sorting lists, for nub

import Utils

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

etymology_solo_suggestions = scaleSuggester (sort $ nub [E,G,D,C,Ds,G,A,C,E,G,Ds,G]) E

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
scalechords sc sig = [ notesBySignature (head sc) c | c <- memoArray, all (`elem` scsig) c]
    where
        scsig = signatureByNotes sc
        memoArray = map (\ m -> map (\ n -> (n + m) `mod` 12) sig) scsig

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
-- TODO: check the signatures again thoroughly
icsRecognizer :: [Note] -> Note -> String
icsRecognizer ns n
    | null ns = "not a pattern"
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
    | sig == sigMode 2 sigScaleM = "SCALE: " ++ show n ++ " dorian; the second mode of " ++ show (toNote $ fromEnum n - 2 ) ++ " major"
    | sig == sigMode 3 sigScaleM = "SCALE: " ++ show n ++ " phrygian; the third mode of " ++ show (toNote $ fromEnum n - 4 ) ++ " major"
    | sig == sigMode 4 sigScaleM = "SCALE: " ++ show n ++ " lydian; the fourth mode of " ++ show (toNote $ fromEnum n - 5 ) ++ " major"
    | sig == sigMode 5 sigScaleM = "SCALE: " ++ show n ++ " mixolydian; the fifth mode of " ++ show (toNote $ fromEnum n - 7 ) ++ " major"
    | sig == sigMode 6 sigScaleM = "SCALE: " ++ show n ++ " natural minor, aka aeolian; the sixth mode of " ++ show (toNote $ fromEnum n - 9 ) ++ " major"
    | sig == sigMode 7 sigScaleM = "SCALE: " ++ show n ++ " locrian; the seventh mode of " ++ show (toNote $ fromEnum n - 11 ) ++ " major"
    -- modes of the melodic minor scale
    | sig == sigMode 2 sigScalemm = "SCALE: " ++ show n ++ " dorian b2, aka phrygian 6, aka javanese, aka phrygidorian; the second mode of " ++ show (toNote $ fromEnum n - 2 ) ++ " melodic minor"
    | sig == sigMode 3 sigScalemm = "SCALE: " ++ show n ++ " lydian augmented, aka lydian #5; the third mode of " ++ show (toNote $ fromEnum n - 3 ) ++ " melodic minor"
    | sig == sigMode 4 sigScalemm = "SCALE: " ++ show n ++ " acoustic, aka overtone, aka lydian b7, aka lydian dominant, aka mixolydian #4, aka lydomyxian; the fourth mode of " ++ show (toNote $ fromEnum n - 5 ) ++ " melodic minor"
    | sig == sigMode 5 sigScalemm = "SCALE: " ++ show n ++ " aeolian dominant, aka melodic major, aka aeolian major, aka mixolydian b6, aka hindu, aka myxaeolian; the fifth mode of " ++ show (toNote $ fromEnum n - 7 ) ++ " melodic minor"
    | sig == sigMode 6 sigScalemm = "SCALE: " ++ show n ++ " half-diminished, aka locrian 2, aka aeolocrian; the sixth mode of " ++ show (toNote $ fromEnum n - 9 ) ++ " melodic minor"
    | sig == sigMode 7 sigScalemm = "SCALE: " ++ show n ++ " altered, aka altered dominant, aka super-locrian, aka locrian b4, aka Pomeroy, aka Ravel, aka diminished whole-tone, aka dominant whole-tone; the seventh mode of " ++ show (toNote $ fromEnum n - 11 ) ++ " melodic minor"
    -- modes of the harmonic minor scale
    | sig == sigMode 2 sigScalemh = "SCALE: " ++ show n ++ " locrian 6; the second mode of " ++ show (toNote $ fromEnum n - 2 ) ++ " harmonic minor"
    | sig == sigMode 3 sigScalemh = "SCALE: " ++ show n ++ " ionian #5; the third mode of " ++ show (toNote $ fromEnum n - 3 ) ++ " harmonic minor"
    | sig == sigMode 4 sigScalemh = "SCALE: " ++ show n ++ " spanish phrygian, aka romanian, aka ukrainian dorian, aka dorian #4; the fourth mode of " ++ show (toNote $ fromEnum n - 4 ) ++ " harmonic minor"
    | sig == sigMode 5 sigScalemh = "SCALE: " ++ show n ++ " phrygian dominant, aka phrygian major, aka altered phrygian, aka dominant b2 b6, aka freygish, aka mixolydian b9 b13 (Berklee); the fifth mode of " ++ show (toNote $ fromEnum n - 7 ) ++ " harmonic minor"
    | sig == sigMode 6 sigScalemh = "SCALE: " ++ show n ++ " lydian #2; the sixth mode of " ++ show (toNote $ fromEnum n - 8 ) ++ " harmonic minor"
    | sig == sigMode 7 sigScalemh = "SCALE: " ++ show n ++ " altered diminished; the seventh mode of " ++ show (toNote $ fromEnum n - 11 ) ++ " harmonic minor"
    -- modes of the harmonic major scale
    | sig == sigMode 2 sigScaleMh = "SCALE: " ++ show n ++ " dorian b5; the second mode of " ++ show (toNote $ fromEnum n - 2 ) ++ " harmonic major"
    | sig == sigMode 3 sigScaleMh = "SCALE: " ++ show n ++ " phrygian b4, aka altered 5; the third mode of " ++ show (toNote $ fromEnum n - 3 ) ++ " harmonic major"
    | sig == sigMode 4 sigScaleMh = "SCALE: " ++ show n ++ " lydian b3, aka lydian minor, aka lydian diminished, aka melodic minor #4; the fourth mode of " ++ show (toNote $ fromEnum n - 4 ) ++ " harmonic major"
    | sig == sigMode 5 sigScaleMh = "SCALE: " ++ show n ++ " mixolydian b2; the fifth mode of " ++ show (toNote $ fromEnum n - 7 ) ++ " harmonic major"
    | sig == sigMode 6 sigScaleMh = "SCALE: " ++ show n ++ " lydian augmented #2, aka aeolian b1; the sixth mode of " ++ show (toNote $ fromEnum n - 8 ) ++ " harmonic major"
    | sig == sigMode 7 sigScaleMh = "SCALE: " ++ show n ++ " locrian b7, aka locrian bb7, aka locrian diminished; the seventh mode of " ++ show (toNote $ fromEnum n - 11 ) ++ " harmonic major"
    -- modes of the double harmonic scale
    | sig == sigMode 2 sigScaledh = "SCALE: " ++ show n ++ " lydian #2 #6; the second mode of " ++ show (toNote $ fromEnum n - 2 ) ++ " double harmonic"
    | sig == sigMode 3 sigScaledh = "SCALE: " ++ show n ++ " ultraphrygian, aka phrygian b4 bb7; the third mode of " ++ show (toNote $ fromEnum n - 4 ) ++ " double harmonic"
    | sig == sigMode 4 sigScaledh = "SCALE: " ++ show n ++ " hungarian minor, aka double harmonic minor, aka harmonic minor #4, aka gypsy minor; the fourth mode of " ++ show (toNote $ fromEnum n - 4 ) ++ " double harmonic"
    | sig == sigMode 5 sigScaledh = "SCALE: " ++ show n ++ " oriental, aka locrian 3 6, aka mixolydian b2 b5; the fifth mode of " ++ show (toNote $ fromEnum n - 7 ) ++ " double harmonic"
    | sig == sigMode 6 sigScaledh = "SCALE: " ++ show n ++ " ionian augmented #2, aka ionian #2 #5; the sixth mode of " ++ show (toNote $ fromEnum n - 8 ) ++ " double harmonic"
    | sig == sigMode 7 sigScaledh = "SCALE: " ++ show n ++ " locrian bb3 bb7; the seventh mode of " ++ show (toNote $ fromEnum n - 11 ) ++ " double harmonic"
    -- mode of the symmetric diminished
    | sig == sigMode 2 sigScalesd = "SCALE: " ++ show n ++ " (half-whole-step) symmetric diminished; the second mode of " ++ show (toNote $ fromEnum n - 1) ++ " symmetric diminished"
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

Fishing out chords for Pan Construction "Death Angel": 

> icsRecognizer [E, F, G, Gs, B, C, D] E
"SCALE: E phrygian b4, aka altered 5; the third mode of Cs harmonic major"

> signatureByNotes [E, F, G, Gs, B, C, D]
[0,1,3,4,7,8,10]

> scalechordRecognizer E [0,1,3,4,7,8,10]
["TRIAD: E major","TRIAD: G major","TRIAD: C major","TRIAD: E minor","TRIAD: F minor","TRIAD: E augmented","TRIAD: Gs augmented","TRIAD: C augmented","TRIAD: F diminished","TRIAD: Gs diminished","TRIAD: B diminished","TRIAD: D diminished","TETRAD: C major seventh","TETRAD: E seventh","TETRAD: G seventh","TETRAD: F minor major seventh","TETRAD: E minor seventh","TETRAD: D half-diminished, aka minor seventh b5","TETRAD: E augmented seventh"]

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
scalechordRecognizer r scalesig = map (\ chord -> icsRecognizer chord (head chord)) $ concatMap (\ chordsig -> scalechords (notesBySignature r scalesig) chordsig) [sigTriadM, sigTriadm, sigTriada, sigTriadd, sig7ChordMM, sig7ChordMm, sig7Chordmm, sig7ChordmM, sig7Chorddm, sig7Chordam]

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
scaleLSuggester ns r = filter (/= "unknown pattern") $ sort $ nub $ map (`icsRecognizer` r) $ scaleSuggester (sort $ nub ns) r

triadLSuggester :: [Note] -> Note -> [String]
triadLSuggester ns r = filter (/= "unknown pattern") $ sort $ nub $ map (`icsRecognizer` r) $ triadSuggester (sort $ nub ns) r
