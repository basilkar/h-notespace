module Main where

import Control.Monad -- for the loop-while in the main quiz
import Data.Maybe
import System.Random
import Guitar
import Note


-- For the following version see https://stackoverflow.com/a/35639761/3749908.

main :: IO ()
main = do
    putStrLn "Choose computation:"
    putStrLn "1 fretboard quizzer"
    putStrLn "2 impro suggester"
    choice <- getLine
    case choice of
        "1" -> quizzer
        "2" -> improSuggester
        _ -> putStr "No computation chosen; exiting"

quizzer :: IO ()
quizzer = do
    let loop = do {g <- newStdGen
        ; let b = head (randoms g :: [Bool])
        ; if b 
            then fretboardQuizOneStandard 
            else fretboardQuizTwoStandard
        ; putStrLn " Hit enter to continue or enter an arbitrary key to exit."
        ; usersays <- getLine
        ; when (usersays == "") loop}
    loop

improSuggester :: IO ()
improSuggester = do
    putStrLn "IMPRO SUGGESTER: What notes do you want to improvize over?"
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
