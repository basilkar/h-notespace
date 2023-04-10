module IOUtils where

import Data.Maybe (fromJust)
import Data.Strings
-- import Text.Regex.TDFA
-- import Text.Regex.TDFA.Text ()

import Note
import SoundNote

charInputsToNods :: [String] -> IO [NOD]
charInputsToNods xs = do
    putStrLn " Enter a nod triple to continue or just hit enter to finish:"
    input <- getLine
    if input == ""
        then return (stripNothing (map charsToNod xs))
        else charInputsToNods (xs ++ [input])

-- incomplete implementation (use regex ^[A-Ga-g][#,s,b]?$)
charsToNod :: String -> Maybe NOD
charsToNod "" = Nothing
charsToNod nodString = Just (fromJust (charsToNote note), read octave, read duration)
    where [note, octave, duration] = strSplitAll "-" nodString

charInputsToNotes :: [String] -> IO [Note]
charInputsToNotes xs = do
    putStrLn " Enter a note to continue or just hit enter to finish:"
    input <- getLine
    if input == ""
        then return (stripNothing (map charsToNote xs))
        else charInputsToNotes (xs ++ [input])

charInputToNote :: String -> IO Note
charInputToNote input = return (fromJust (charsToNote input))

stripNothing :: [Maybe a] -> [a]
stripNothing [] = []
stripNothing (Nothing:xs) = stripNothing xs
stripNothing ((Just x):xs) = x: stripNothing xs

charsToNote :: String -> Maybe Note
charsToNote chars = case chars of
    "A" -> Just A
    "a" -> Just A
    "As" -> Just As
    "as" -> Just As
    "A#" -> Just As
    "a#" -> Just As
    "Bb" -> Just As
    "bb" -> Just As
    "B" -> Just B
    "b" -> Just B
    "C" -> Just C
    "c" -> Just C
    "Cs" -> Just Cs
    "cs" -> Just Cs
    "C#" -> Just Cs
    "c#" -> Just Cs
    "Db" -> Just Cs
    "db" -> Just Cs
    "D" -> Just D
    "d" -> Just D
    "Ds" -> Just Ds
    "ds" -> Just Ds
    "D#" -> Just Ds
    "d#" -> Just Ds
    "Eb" -> Just Ds
    "eb" -> Just Ds
    "E" -> Just E
    "e" -> Just E
    "F" -> Just F
    "f" -> Just F
    "Fs" -> Just Fs
    "fs" -> Just Fs
    "F#" -> Just Fs
    "f#" -> Just Fs
    "Gb" -> Just Fs
    "gb" -> Just Fs
    "G" -> Just G
    "g" -> Just G
    "Gs" -> Just Gs
    "gs" -> Just Gs
    "G#" -> Just Gs
    "g#" -> Just Gs
    "Ab" -> Just Gs
    "ab" -> Just Gs
    _ -> Nothing