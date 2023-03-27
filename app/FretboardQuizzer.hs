module FretboardQuizzer where

import Control.Monad -- for the loop-while in the main quiz
import System.Random

import Guitar

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