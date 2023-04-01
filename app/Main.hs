module Main where

import Control.Monad -- for the loop-while in the main quiz
import Data.Maybe
import System.Random

import FretboardQuizzer
import ImproSuggester
import Recognizer

main :: IO ()
main = do
    putStrLn "Welcome to notespace, the music calculator."
    putStrLn "In the applications below you enter notes as, e.g., Fs, F#, fs, f#, Gb or gb."
    let loop = do {
        putStrLn "Choose application (or 0 to exit):"
        ; putStrLn "1 impro suggester"
        ; putStrLn "2 namer"
        ; putStrLn "3 scale chord finder"
        ; putStrLn "4 known scale chord finder"
        ; putStrLn "5 fretboard quizzer"
        ; choice <- getLine
        ; case choice of
              "0" -> putStrLn "Exiting."
              "1" -> do {ImproSuggester.improSuggester ; loop}
              "2" -> do {Recognizer.namer; loop}
              "3" -> do {Recognizer.scaleChordFinder; loop}
              "4" -> do {Recognizer.knownScaleChordFinder; loop}
              "5" -> do {FretboardQuizzer.fretboardQuizzer ; loop}
              _ -> do {putStr "No valid application chosen." ; loop}}
    loop