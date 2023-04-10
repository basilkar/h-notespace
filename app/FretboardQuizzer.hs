module FretboardQuizzer where

import Control.Monad (when)
import System.Random (Random, newStdGen, random, randomR, randoms)

import IOUtils (charInputToNote)

import Guitar (Fret, Guitarstring, fretboard, tuningStandard)
import Note (Note)

fretboardQuizzer :: IO ()
fretboardQuizzer = do
    let loop = do {g <- newStdGen
        ; let b = head (randoms g :: [Bool])
        ; if b
            then fretboardQuizOneStandard
            else fretboardQuizTwoStandard
        ; putStrLn " Hit enter to continue or enter an arbitrary key to exit."
        ; usersays <- getLine
        ; when (usersays == "") loop}
    loop

instance Random Guitarstring where
  randomR (string,string') generator = case randomR (fromEnum string, fromEnum string') generator of
    (stringNumber, generator') -> (toEnum stringNumber, generator')
  random = randomR (minBound,maxBound)

instance Random Fret where
  randomR (fret,fret') generator = case randomR (fromEnum fret, fromEnum fret') generator of
    (fretNumber, generator') -> (toEnum fretNumber, generator')
  random = randomR (minBound,maxBound)

instance Random Note where
  randomR (note,note') generator = case randomR (fromEnum note, fromEnum note') generator of
    (noteNumber, generator') -> (toEnum noteNumber, generator')
  random = randomR (minBound,maxBound)

fretboardQuizOneStandard :: IO ()
fretboardQuizOneStandard = do
  g1 <- newStdGen
  g2 <- newStdGen
  let fret = head (randoms g1 :: [Fret])
  let string = head (randoms g2 :: [Guitarstring])
  let answer = fretboard tuningStandard string fret
  putStrLn ("FRETBOARD QUIZZER: What is the note on fret " ++ show (fromEnum fret) ++ " of string " ++ show (fromEnum string + 1) ++ " in standard tuning?")
  charsGuess <- getLine
  guess <- charInputToNote charsGuess
  if guess == answer then putStr "FRETBOARD QUIZZER: Correct!" else putStr ("FRETBOARD QUIZZER: Wrong! The note is actually " ++ show answer ++ ".")

fretboardQuizTwoStandard :: IO ()
fretboardQuizTwoStandard = do
  g1 <- newStdGen
  g2 <- newStdGen
  let note = head (randoms g1 :: [Note])
  let string = head (randoms g2 :: [Guitarstring])
  putStrLn ("FRETBOARD QUIZZER: Where is the note " ++ show note ++ " on string " ++ show (fromEnum string + 1) ++ " in standard tuning?")
  guess <- readLn --getLine
  if fretboard tuningStandard string (toEnum guess) == note then putStr "FRETBOARD QUIZZER: Correct!" else putStr "FRETBOARD QUIZZER: Wrong." -- Needs some more work to also provide the user with the correct answer here.