module ImproSuggester where

import Data.Maybe

import Note

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
    putStrLn "IMPRO SUGGESTER: Here are scales to use:"
    print (scaleSuggester notes root)
    putStrLn "IMPRO SUGGESTER: Here are triads to use:"
    print (triadSuggester notes root)

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
