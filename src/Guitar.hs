module Guitar where

import Note (Note(..), halfsteps)

data Guitarstring = StringOne | StringTwo | StringThree | StringFour | StringFive | StringSix
  deriving (Eq, Enum, Ord, Show, Read, Bounded)

data Fret = FretZero | FretOne | FretTwo | FretThree | FretFour | FretFive | FretSix | FretSeven | FretEight | FretNine | FretTen | FretEleven | FretTwelve | FretThirteen | FretFourteen | FretFifteen | FretSixteen | FretSeventeen | FretEighteen | FretNineteen| FretTwenty| FretTwentyone| FretTwentytwo
  deriving (Eq, Enum, Ord, Show, Read, Bounded)

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


type Tuning = Guitarstring -> Note

fretboard :: Tuning -> Guitarstring -> Fret -> Note
fretboard tuning string fret = halfsteps (tuning string) (fromEnum fret)