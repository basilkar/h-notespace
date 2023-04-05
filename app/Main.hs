module Main where

import Control.Monad -- for the loop-while in the main quiz
import Data.Maybe
import System.Random

import FretboardQuizzer
import ImproSuggester

main :: IO ()
main = do
    putStrLn "Choose computation:"
    putStrLn "1 fretboard quizzer"
    putStrLn "2 impro suggester"
    choice <- getLine
    case choice of
        "1" -> FretboardQuizzer.fretboardQuizzer
        "2" -> ImproSuggester.improSuggester
        _ -> putStr "No computation chosen; exiting"
