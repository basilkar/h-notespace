module ImproSuggester where

import Data.List (nub, sort, sortOn)

import Note
import Utils (levenshtein)

import IOUtils (charInputToNote, charInputsToNotes)
import Recognizer (icsRecognizer)


improSuggester :: IO ()
improSuggester = do
    putStrLn "IMPRO SUGGESTER: What notes do you want to improvize over?"
    let inputs = []
    notes <- charInputsToNotes inputs
    putStrLn "IMPRO SUGGESTER: You chose the notes:"
    print notes
    putStrLn "IMPRO SUGGESTER: What would be the root?"
    charsRoot <- getLine
    root <- charInputToNote charsRoot
    putStrLn "IMPRO SUGGESTER: Do you want to see the suggestions by name? (y/n)"
    literate <- getLine
    putStrLn "IMPRO SUGGESTER: Here are scales to use:"
    if literate == "y"
      then mapM_ print (scaleLSuggester notes root)
      else mapM_ print (scaleSuggester notes root)
    putStrLn "IMPRO SUGGESTER: Here are triads to use:"
    if literate == "y"
      then mapM_ print (triadLSuggester notes root)
      else mapM_ print (triadSuggester notes root)

{-

Suggest scales or triads fitting to a set of notes.

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

-- Now that we have a recognizer, we can also have a version of suggester (see application 1.1) where we get the names of the suggested scales or chords, rather than the note lists themselves.

scaleLSuggester :: [Note] -> Note -> [String]
scaleLSuggester ns r = filter (/= "unknown pattern") $ nub $ map (`icsRecognizer` r) $ scaleSuggester (sort $ nub ns) r

triadLSuggester :: [Note] -> Note -> [String]
triadLSuggester ns r = filter (/= "unknown pattern") $ nub $ map (`icsRecognizer` r) $ triadSuggester (sort $ nub ns) r