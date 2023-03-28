module ImproSuggester where

import Data.List -- for sorting lists, for nub
import Data.Maybe

import Note
import Utils

import Recognizer


improSuggester :: IO ()
improSuggester = do
    putStrLn "IMPRO SUGGESTER: What notes do you want to improvize over? (e.g., A, B, Cs,...)"
    let inputs = []
    notes <- charInputsToNotes inputs
    putStrLn "IMPRO SUGGESTER: You chose the notes:"
    print notes
    putStrLn "IMPRO SUGGESTER: What would be the root? (e.g., E)"
    charsRoot <- getLine
    root <- charInputToNote charsRoot
    putStrLn "IMPRO SUGGESTER: Do you want to see the suggestions by name? (y/n)"
    literate <- getLine
    putStrLn "IMPRO SUGGESTER: Here are scales to use:"
    if literate == "y"
      then print (scaleLSuggester notes root)
      else print (scaleSuggester notes root)
    putStrLn "IMPRO SUGGESTER: Here are triads to use:"
    if literate == "y"
      then print (triadLSuggester notes root)
      else print (triadSuggester notes root)

charInputsToNotes :: [String] -> IO [Note]
charInputsToNotes xs = do
    putStrLn "IMPRO SUGGESTER: Enter a note to continue or just hit enter to finish:"
    input <- getLine
    if input == ""
        then return (stripNothing (map charsToNote xs))
        else charInputsToNotes (input : xs)

charInputToNote :: String -> IO (Note)
charInputToNote input = return (fromJust (charsToNote input))

stripNothing :: [Maybe a] -> [a]
stripNothing [] = []
stripNothing (Nothing:xs) = stripNothing xs
stripNothing ((Just x):xs) = x: stripNothing xs

charsToNote :: String -> Maybe Note
charsToNote chars = case chars of
    "A" -> Just A
    "As" -> Just As
    "Bb" -> Just As
    "B" -> Just B
    "C" -> Just C
    "Cs" -> Just Cs
    "Db" -> Just Cs
    "D" -> Just D
    "Ds" -> Just Ds
    "Eb" -> Just Ds
    "E" -> Just E
    "F" -> Just F
    "Fs" -> Just Fs
    "Gb" -> Just Fs
    "G" -> Just G
    "Gs" -> Just Gs
    "Ab" -> Just Gs
    _ -> Nothing

{---------------------------------------}
{- 1.1 -- APPLICATION: IMPRO SUGGESTER -}
{---------------------------------------}

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

etymology_solo_suggestions = scaleSuggester (sort $ nub [E,G,D,C,Ds,G,A,C,E,G,Ds,G]) E

> etymology_solo_suggestions
[[A,C,D,Ds,E,G],[E,G,A,C,D],[E,Fs,Gs,B,Cs],[E,Fs,A,B,D],[E,Fs,A,B,Cs],[E,G,A,B,D],[E,Fs,Gs,As,C,D],[E,Fs,Gs,A,C,Cs,Ds],[E,Fs,Gs,A,B,Cs,Ds],[E,Fs,G,A,B,Cs,D],[E,F,G,A,B,C,D],[E,Fs,Gs,As,B,Cs,Ds],[E,Fs,Gs,A,B,Cs,D],[E,Fs,G,A,B,C,D],[E,F,G,A,As,C,D],[E,Fs,G,A,B,C,Ds],[E,F,G,A,As,Cs,D],[E,Fs,G,As,B,Cs,D],[E,F,Gs,A,B,C,D],[E,G,Gs,As,B,Cs,Ds],[E,F,G,Gs,As,C,Cs],[E,Fs,G,A,B,Cs,Ds],[E,F,G,A,B,Cs,D],[E,Fs,Gs,As,C,Cs,Ds],[E,Fs,Gs,As,B,Cs,D],[E,Fs,Gs,A,B,C,D],[E,Fs,G,A,As,C,D],[E,F,G,Gs,As,C,D],[E,F,Fs,G,Gs,A,As,B,C,Cs,D,Ds]]

> length etymology_solo_suggestions
29

E aeolian flat five (aka half diminished, aka locrian natural 2) is indeed among these suggestions.

e_aeolian_flat_five = mode 6 scalemm G

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

{--------------------------------------------}
{- 1.5 -- APPLICATION: A LITERATE SUGGESTER -}
{--------------------------------------------}

-- Now that we have a recognizer, we can also have a version of suggester (see application 1.1) where we get the names of the suggested scales or chords, rather than the note lists themselves.

scaleLSuggester :: [Note] -> Note -> [String]
scaleLSuggester ns r = filter (/= "unknown pattern") $ sort $ nub $ map (`icsRecognizer` r) $ scaleSuggester (sort $ nub ns) r

triadLSuggester :: [Note] -> Note -> [String]
triadLSuggester ns r = filter (/= "unknown pattern") $ sort $ nub $ map (`icsRecognizer` r) $ triadSuggester (sort $ nub ns) r
