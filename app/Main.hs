module Main where

import Control.Monad -- for the loop-while in the main quiz
import Data.Maybe
import System.Random

import FretboardQuizzer
import ImproSuggester

main :: IO ()
main = do
    putStrLn "Welcome to notespace."
    let loop = do {
        putStrLn "Choose computation:"
        ; putStrLn "0 exit"
        ; putStrLn "1 impro suggester"
        ; putStrLn "2 fretboard quizzer"
        ; choice <- getLine
        ; case choice of
              "0" -> putStrLn "Exiting."
              "1" -> do {ImproSuggester.improSuggester ; loop}
              "2" -> do {FretboardQuizzer.fretboardQuizzer ; loop}
              _ -> do {putStr "No valid computation chosen." ; loop}}
    loop