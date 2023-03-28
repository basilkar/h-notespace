module Recognizer where

import Data.List -- for sorting lists, for nub

import ScaleChordsFinder
import IOUtils

import Note


recognizer :: IO ()
recognizer = do
    putStrLn "RECOGNIZER: What notes do you want to recognize?"
    let inputs = []
    notes <- charInputsToNotes inputs
    putStrLn "RECOGNIZER: You chose the notes:"
    print notes
    putStrLn "RECOGNIZER: What would be the root?"
    charsRoot <- getLine
    root <- charInputToNote charsRoot
    putStr "RECOGNIZER: "
    print (icsRecognizer notes root)

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