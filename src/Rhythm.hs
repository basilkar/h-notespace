{---------------------}
{- 3 -- ABOUT RHYTHM -}
{---------------------}

module Rhythm where

import Math.Combinat
import Data.List

-- We may view time signatures like 4/4, 3/4, et cetera, as pairs of integers.
type TimeSig = (Int,Int) -- well, the integers are supposed to be positive

-- Given a time signature and a number of bars, give all rhythmic patterns for the whole span of bars.

rhythmPatterns :: (Int,Int) -> Int -> [[Bool]]
rhythmPatterns tsig b = [map (\ x -> x `elem` (numPatterns !! i)) allBeats | i <- [0..(len2-1)]] {-[map (\ x -> elem x (numPatterns !! i)) allBeats | i <- [0..len]]-} {-[ [elem x (numPatterns !! i) | x <- allBeats] | i <- [0..len]]-}
    where
        len = length allBeats
        numPatterns = concat [choose i allBeats | i <- [0..len]]
        allBeats = [1..(b * fst tsig)]
        len2 = length numPatterns

-- The following yields all rhythm patterns that stress exactly m many beats of all

rhythmPatternsWith :: (Int, Int) -> Int -> Int -> [[Bool]]
rhythmPatternsWith tsig b m = filter ([False | i <- [1..(b * fst tsig - m)]] `isSubsequenceOf`) $ filter ([True | i <- [1..m]] `isSubsequenceOf` ) rpats
    where rpats = rhythmPatterns tsig b

{- EXAMPLES -}

{-

Say we want to practice on accentuating 2 out of 4 beats, i.e., out of one bar of a typical 4/4 song.

> rhythmPatternsWith (4,4) 1 2
[[True,True,False,False],[True,False,True,False],[True,False,False,True],[False,True,True,False],[False,True,False,True],[False,False,True,True]]

Here are all ways to accentuate three out of eight eighths.

> rhythmPatternsWith (8,8) 1 3
[[True,True,True,False,False,False,False,False],[True,True,False,True,False,False,False,False],[True,True,False,False,True,False,False,False],[True,True,False,False,False,True,False,False],[True,True,False,False,False,False,Tr
ue,False],[True,True,False,False,False,False,False,True],[True,False,True,True,False,False,False,False],[True,False,True,False,True,False,False,False],[True,False,True,False,False,True,False,False],[True,False,True,False,False
,False,True,False],[True,False,True,False,False,False,False,True],[True,False,False,True,True,False,False,False],[True,False,False,True,False,True,False,False],[True,False,False,True,False,False,True,False],[True,False,False,T
rue,False,False,False,True],[True,False,False,False,True,True,False,False],[True,False,False,False,True,False,True,False],[True,False,False,False,True,False,False,True],[True,False,False,False,False,True,True,False],[True,Fals
e,False,False,False,True,False,True],[True,False,False,False,False,False,True,True],[False,True,True,True,False,False,False,False],[False,True,True,False,True,False,False,False],[False,True,True,False,False,True,False,False],[
False,True,True,False,False,False,True,False],[False,True,True,False,False,False,False,True],[False,True,False,True,True,False,False,False],[False,True,False,True,False,True,False,False],[False,True,False,True,False,False,True
,False],[False,True,False,True,False,False,False,True],[False,True,False,False,True,True,False,False],[False,True,False,False,True,False,True,False],[False,True,False,False,True,False,False,True],[False,True,False,False,False,
True,True,False],[False,True,False,False,False,True,False,True],[False,True,False,False,False,False,True,True],[False,False,True,True,True,False,False,False],[False,False,True,True,False,True,False,False],[False,False,True,Tru
e,False,False,True,False],[False,False,True,True,False,False,False,True],[False,False,True,False,True,True,False,False],[False,False,True,False,True,False,True,False],[False,False,True,False,True,False,False,True],[False,False
,True,False,False,True,True,False],[False,False,True,False,False,True,False,True],[False,False,True,False,False,False,True,True],[False,False,False,True,True,True,False,False],[False,False,False,True,True,False,True,False],[Fa
lse,False,False,True,True,False,False,True],[False,False,False,True,False,True,True,False],[False,False,False,True,False,True,False,True],[False,False,False,True,False,False,True,True],[False,False,False,False,True,True,True,F
alse],[False,False,False,False,True,True,False,True],[False,False,False,False,True,False,True,True],[False,False,False,False,False,True,True,True]]
*Main>

Lacking a gui, it would be nice to have a pretty-print of this function too---although the complexity is, of course, what it is---ma non adesso.

-}