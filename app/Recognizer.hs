module Recognizer where

import Data.List -- for sorting lists, for nub

import IOUtils

import Note


namer :: IO ()
namer = do
    putStrLn "RECOGNIZER: What notes do you want to name?"
    let inputs = []
    notes <- charInputsToNotes inputs
    putStrLn "RECOGNIZER: You chose the notes:"
    print notes
    putStrLn "RECOGNIZER: What would be the root?"
    charsRoot <- getLine
    root <- charInputToNote charsRoot
    putStr "RECOGNIZER: "
    print (icsRecognizer notes root)


scaleChordFinder :: IO ()
scaleChordFinder = do
    putStrLn "RECOGNIZER: In what notes do you want to search for chords?"
    let inputs = []
    notes <- charInputsToNotes inputs
    putStrLn "RECOGNIZER: You chose the notes:"
    print notes
    putStrLn "RECOGNIZER: What is an example of a chord whose kind you are looking for in these notes?"
    let exampleInputs = []
    exampleNotes <- charInputsToNotes exampleInputs
    putStrLn "RECOGNIZER: You chose to look for chords like:"
    print exampleNotes
    let chords = scaleChords notes (signatureByNotes exampleNotes)
    if not (null chords)
        then do {
            putStrLn "RECOGNIZER: Here are chords like this in the notes you've provided:"
            ; mapM_ print chords
            }
        else putStrLn "RECOGNIZER: No chords like this were found in the notes you've provided."


knownScaleChordFinder :: IO ()
knownScaleChordFinder = do
    putStrLn "RECOGNIZER: In what notes do you want to search for known chords?"
    let inputs = []
    notes <- charInputsToNotes inputs
    putStrLn "RECOGNIZER: You chose the notes:"
    print notes

    let chordNames = scaleChordRecognizerFromNotes notes
    if not (null chordNames)
        then do {
            putStrLn "RECOGNIZER: Here are known chords in the notes you've provided:"
            ; mapM_ print chordNames
            }
        else putStrLn "RECOGNIZER: No known chords were found in the notes you've provided."

{-

Produce the chords of a given scale, according to a given signature.

INPUT a scale and a signature
OUTPUT a list of lists of notes from the given scale that fit the given signature

-}

scaleChords :: [Note] -> [Int] -> [[Note]]
scaleChords [] _ = []
scaleChords _ [] = []
scaleChords sc sig = [ notesBySignature (head sc) c | c <- memoArray, all (`elem` scsig) c]
    where
        scsig = signatureByNotes sc
        memoArray = map (\ m -> map (\ n -> (n + m) `mod` 12) sig) scsig

{-

Recognize a rooted sequence of notes (meaning, a sequence of notes together with a note that is to be perceived as the root of the musical pattern) as an interval, chord, or scale.

INPUT a list of notes and a note
OUTPUT a list of lists of notes from the given scale that fit the given signature

-}
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
    | sig == sig7ChordmM = "TETRAD: " ++ show n ++ " minor major seventh"
    | sig == sig7Chordmm = "TETRAD: " ++ show n ++ " minor seventh"
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

{-

INPUT a note (for the root) and a signature (for the type of scale)
OUTPUT a list of strings indicating all triads and all tetrads to be found within the induced scale

To do this, we combine applications 1.2 and 1.3.

-}

scaleChordRecognizer :: Note -> [Int] -> [String]
scaleChordRecognizer r scalesig = map (\ chord -> icsRecognizer chord (head chord)) $ concatMap (scaleChords (notesBySignature r scalesig)) [sigTriadM, sigTriadm, sigTriada, sigTriadd, sig7ChordMM, sig7ChordMm, sig7Chordmm, sig7ChordmM, sig7Chorddm, sig7Chordam]

scaleChordRecognizerFromNotes :: [Note] -> [String]
scaleChordRecognizerFromNotes notes = map (\ chord -> icsRecognizer chord (head chord)) $ concatMap (scaleChords notes) [sigTriadM, sigTriadm, sigTriada, sigTriadd, sig7ChordMM, sig7ChordMm, sig7Chordmm, sig7ChordmM, sig7Chorddm, sig7Chordam]