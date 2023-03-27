{------------------------------}
{- 5 -- CONCERNING THE GUITAR -}
{------------------------------}

module Guitar where

{-
Watching this guy here, https://www.youtube.com/watch?v=b9pYEjZ4l48, it seemed reasonable to start implementing some of the quirks that are particular to the guitar.
-}

-- Start by introducing two basic datatypes, one for the strings and one for the frets.

import System.Random
import Note
data Guitarstring = StringOne | StringTwo | StringThree | StringFour | StringFive | StringSix
  deriving (Eq, Enum, Ord, Show, Read, Bounded)

data Fret = FretZero | FretOne | FretTwo | FretThree | FretFour | FretFive | FretSix | FretSeven | FretEight | FretNine | FretTen | FretEleven | FretTwelve | FretThirteen | FretFourteen | FretFifteen | FretSixteen | FretSeventeen | FretEighteen | FretNineteen| FretTwenty| FretTwentyone| FretTwentytwo
  deriving (Eq, Enum, Ord, Show, Read, Bounded)

-- Then a couple of the common tunings.

tuningStandard :: Guitarstring -> Note
tuningStandard string = case string of
  StringOne -> E
  StringTwo -> B
  StringThree -> G
  StringFour -> D
  StringFive -> A
  StringSix -> E

tuningDropD :: Guitarstring -> Note
tuningDropD string = case string of
  StringOne -> E
  StringTwo -> B
  StringThree -> G
  StringFour -> D
  StringFive -> A
  StringSix -> D

tuningOpenD :: Guitarstring -> Note
tuningOpenD string = case string of
  StringOne -> D
  StringTwo -> A
  StringThree -> Fs
  StringFour -> D
  StringFive -> A
  StringSix -> D

tuningDadgad :: Guitarstring -> Note
tuningDadgad string = case string of
  StringOne -> D
  StringTwo -> A
  StringThree -> G
  StringFour -> D
  StringFive -> A
  StringSix -> D

tuningNST :: Guitarstring -> Note
tuningNST string = case string of
  StringOne -> G
  StringTwo -> E
  StringThree -> A
  StringFour -> D
  StringFive -> G
  StringSix -> C

-- Just a grain of sugar.

type Tuning = Guitarstring -> Note

-- And now, the fretboard; given a tuning, we allot notes to the pairs of strings and frets.

fretboard :: Tuning -> Guitarstring -> Fret -> Note
fretboard tuning string fret = halfsteps (tuning string) (fromEnum fret)

{-----------------------------------------}
{- 5.1 -- APPLICATION: FRETBOARD QUIZZES -}
{-----------------------------------------}

{-
We can now implement the quizzes that the youtube guy from above suggested. For this, we will need to use the IO monad. We will also need the Random module. See https://wiki.haskell.org/Introduction_to_IO, https://wiki.haskell.org/IO_inside and  https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/randoms.
-}

-- To be able to generate random strings, frets, or notes for the needs of the quiz, we need to make the respective types into instances of the class Random.

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

-- Now we can define the quizzes as follows.

fretboardQuizOneStandard :: IO ()
fretboardQuizOneStandard = do
  g1 <- newStdGen
  g2 <- newStdGen
  let fret = head (randoms g1 :: [Fret])
  let string = head (randoms g2 :: [Guitarstring])
  let answer = fretboard tuningStandard string fret
  putStrLn ("FRETBOARD QUIZZER: What is the note on fret " ++ show (fromEnum fret) ++ " of string " ++ show (fromEnum string + 1) ++ " in standard tuning?")
  putStrLn " nomenclature: A, As, B, C, Cs, D, ..."
  guess <- getLine
  if guess == show answer then putStr "FRETBOARD QUIZZER: Correct!" else putStr ("FRETBOARD QUIZZER: Wrong! The note is actually " ++ show answer ++ ".")

fretboardQuizTwoStandard :: IO ()
fretboardQuizTwoStandard = do
  g1 <- newStdGen
  g2 <- newStdGen
  let note = head (randoms g1 :: [Note])
  let string = head (randoms g2 :: [Guitarstring])
  putStrLn ("FRETBOARD QUIZZER: Where is the note " ++ show note ++ " on string " ++ show (fromEnum string + 1) ++ " in standard tuning?")
  guess <- readLn --getLine
  if fretboard tuningStandard string (toEnum guess) == note then putStr "FRETBOARD QUIZZER: Correct!" else putStr "FRETBOARD QUIZZER: Wrong." -- Needs some more work to also provide the user with the correct answer here.