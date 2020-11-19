module Main where

import Control.Monad -- for the loop-while in the main quiz
import System.Random
import Guitar


-- For the following version see https://stackoverflow.com/a/35639761/3749908.

main :: IO ()
main = do
    let loop = do {g <- newStdGen
        ; let b = head (randoms g :: [Bool])
        ; if b then fretboardQuizOneStandard else fretboardQuizTwoStandard
        ; putStrLn " Hit enter to continue or enter an arbitrary key to exit."
        ; usersays <- getLine
        ; when (usersays == "") loop}
    loop


-- This works already:

-- main :: IO ()
-- main = do
--   g <- newStdGen
--   let b = head (randoms g :: [Bool])
--   if b then (fretboardQuizOneStandard) else (fretboardQuizTwoStandard)

-- In an exactly similar manner we may define quizzes for other tunings. Of course, it'd be even better to let the user decide which tuning they want to be quized on. Ma non adesso.

-- while :: Bool -> IO () -> IO ()
-- while False _ = return ()
-- while True a = do a
