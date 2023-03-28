module Main where

import Control.Monad -- for the loop-while in the main quiz
import Data.Maybe
import System.Random

import FretboardQuizzer
import ImproSuggester

main :: IO ()
main = do
    putStrLn "Choose computation:"
    putStrLn "1 impro suggester"
    putStrLn "2 fretboard quizzer"
    choice <- getLine
    case choice of
        "1" -> ImproSuggester.improSuggester
        "2" -> FretboardQuizzer.fretboardQuizzer
        _ -> putStr "No computation chosen; exiting"
