module Main where

import FretboardQuizzer (fretboardQuizzer)
import ImproSuggester (improSuggester)
import Player (player)
import Recognizer (knownScaleChordFinder, namer, scaleChordFinder)

main :: IO ()
main = do
    putStrLn "This is notespace, a music calculator."
    putStrLn "In the applications below you enter notes as, e.g., Fs, F#, fs, f#, Gb or gb."
    let loop = do {
        putStrLn "Choose application (or 0 to exit):"
        ; putStrLn "1 impro suggester"
        ; putStrLn "2 namer"
        ; putStrLn "3 scale chord finder"
        ; putStrLn "4 known scale chord finder"
        ; putStrLn "5 player"
        ; putStrLn "6 fretboard quizzer"
        ; choice <- getLine
        ; case choice of
              "0" -> putStrLn "Exiting."
              "1" -> do {ImproSuggester.improSuggester ; loop}
              "2" -> do {Recognizer.namer ; loop}
              "3" -> do {Recognizer.scaleChordFinder ; loop}
              "4" -> do {Recognizer.knownScaleChordFinder ; loop}
              "5" -> do {Player.player ; loop}
              "6" -> do {FretboardQuizzer.fretboardQuizzer ; loop}
              _ -> do {putStr "No valid application chosen. " ; loop}}
    loop