module Rhythm where

import Data.List (isSubsequenceOf)
import Math.Combinat (choose)

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