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
    let loop = do {
        putStrLn "Choose calculation (or 0 to exit):"
        ; putStrLn "1 impro suggester"
        ; putStrLn "2 recognizer"
        ; putStrLn "3 fretboard quizzer"
        ; choice <- getLine
        ; case choice of
              "0" -> putStrLn "Exiting."
              "1" -> do {ImproSuggester.improSuggester ; loop}
              "2" -> do {Recognizer.recognizer; loop}
              "3" -> do {FretboardQuizzer.fretboardQuizzer ; loop}
              _ -> do {putStr "No valid computation chosen." ; loop}}
    loop