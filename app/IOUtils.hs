module IOUtils where

import Data.Maybe

import Note

charInputsToNotes :: [String] -> IO [Note]
charInputsToNotes xs = do
    putStrLn " Enter a note to continue or just hit enter to finish:"
    input <- getLine
    if input == ""
        then return (stripNothing (map charsToNote xs))
        else charInputsToNotes (xs ++ [input])

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
    "A#" -> Just As
    "Bb" -> Just As
    "B" -> Just B
    "C" -> Just C
    "Cs" -> Just Cs
    "C#" -> Just Cs
    "Db" -> Just Cs
    "D" -> Just D
    "Ds" -> Just Ds
    "D#" -> Just Ds
    "Eb" -> Just Ds
    "E" -> Just E
    "F" -> Just F
    "Fs" -> Just Fs
    "F#" -> Just Fs
    "Gb" -> Just Fs
    "G" -> Just G
    "Gs" -> Just Gs
    "G#" -> Just Gs
    "Ab" -> Just Gs
    _ -> Nothing