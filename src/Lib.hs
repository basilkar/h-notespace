module Lib where

import Data.List -- for sorting lists, for nub
import Math.Combinat -- for rhythmic exercises
import System.Random -- for the guitar fretboard quizzes

import Notes
import Utils


{--------------------------------------------------}
{- 2 -- AN INDUCTIVE TYPE FOR CHORD CONSTRUCTIONS -}
{--------------------------------------------------}

{-
So, one way to speak about chords is to view them as values of [Note], exactly as we did above with the four basic types of triads.
-}

type ChordL = [Note]

{-
Another way to capture all chords, which tracks their actual construction (as is sometimes the case in harmonic analysis), including single notes, intervals, triads, and polychords, is to use an inductive type, making use of the previously defined Note datatype. Here we restrict the chord datatype to up to "pentads"; not sure if we need more or less.
-}

data Chord = ChordBot | Single Note | Dyad Chord Chord | Triad Chord Chord Chord | Tetrad Chord Chord Chord Chord | Pentad Chord Chord Chord Chord Chord
    deriving (Eq, Ord, Show, Read)

{- EXAMPLES -}

e_minor_over_c = Dyad (Single C) (Triad (Single E) (Single G) (Single B))
a_over_c_major = Dyad (Triad (Single C) (Single E) (Single G)) (Single A)
c_major_extended = Dyad (Triad (Single C) (Single E) (Single G)) (ChordBot)

{-

> :t a_over_c_major
a_over_c_major :: Chord

> :t C
C :: Note

> :t Single C
Single C :: Chord

-}

{-
There is a bunch of functions to be defined on any inductive datatype. Note that the constructor "Single" takes values of "Note" as arguments, and it seems intuitive to view it as nullary in (some of?) the definitions that follow.
-}

-- The size and the height of a token (i.e., a chord).

sizeOfChordToken :: Chord -> Int
sizeOfChordToken ChordBot           = 0
sizeOfChordToken (Single _)         = 1
sizeOfChordToken (Dyad c d)         = 1 + (sizeOfChordToken c) + (sizeOfChordToken d)
sizeOfChordToken (Triad c d e)      = 1 + (sizeOfChordToken c) + (sizeOfChordToken d) + (sizeOfChordToken e)
sizeOfChordToken (Tetrad c d e f)   = 1 + (sizeOfChordToken c) + (sizeOfChordToken d) + (sizeOfChordToken e) + (sizeOfChordToken f)
sizeOfChordToken (Pentad c d e f g) = 1 + (sizeOfChordToken c) + (sizeOfChordToken d) + (sizeOfChordToken e) + (sizeOfChordToken f) + (sizeOfChordToken g)

heightOfChordToken :: Chord -> Int
heightOfChordToken ChordBot             = 0
heightOfChordToken (Single _)           = 1
heightOfChordToken (Dyad c d)           = 1 + maximum [heightOfChordToken c, heightOfChordToken d]
heightOfChordToken (Triad c d e)        = 1 + maximum [heightOfChordToken c, heightOfChordToken d, heightOfChordToken e]
heightOfChordToken (Tetrad c d e f)     = 1 + maximum [heightOfChordToken c, heightOfChordToken d, heightOfChordToken e, heightOfChordToken f]
heightOfChordToken (Pentad c d e f g)   = 1 + maximum [heightOfChordToken c, heightOfChordToken d, heightOfChordToken e, heightOfChordToken f, heightOfChordToken g]

-- The size of the chord, that is, its number of notes.

sizeOfChord :: Chord -> Int
sizeOfChord ChordBot            = 0
sizeOfChord (Single _)          = 1
sizeOfChord (Dyad c d)          = (sizeOfChord c) + (sizeOfChord d)
sizeOfChord (Triad c d e)       = (sizeOfChord c) + (sizeOfChord d) + (sizeOfChord e)
sizeOfChord (Tetrad c d e f)    = (sizeOfChord c) + (sizeOfChord d) + (sizeOfChord e) + (sizeOfChord f)
sizeOfChord (Pentad c d e f g)  = (sizeOfChord c) + (sizeOfChord d) + (sizeOfChord e) + (sizeOfChord f) + (sizeOfChord g)

-- The yield of a token, that is, its leaves, that is, its actual notes.

yieldOfChord :: Chord -> [Note]
yieldOfChord ChordBot           = []
yieldOfChord (Single n)         = [n]
yieldOfChord (Dyad c d)         = (yieldOfChord c) ++ (yieldOfChord d)
yieldOfChord (Triad c d e)      = (yieldOfChord c) ++ (yieldOfChord d) ++ (yieldOfChord e)
yieldOfChord (Tetrad c d e f)   = (yieldOfChord c) ++ (yieldOfChord d) ++ (yieldOfChord e) ++ (yieldOfChord f)
yieldOfChord (Pentad c d e f g) = (yieldOfChord c) ++ (yieldOfChord d) ++ (yieldOfChord e) ++ (yieldOfChord f) ++ (yieldOfChord g)

{- EXAMPLES

> sizeOfChordToken e_minor_over_c
6

> heightOfChordToken e_minor_over_c
3

> sizeOfChord e_minor_over_c
4

> yieldOfChord e_minor_over_c
[C,E,G,B]

> yieldOfChord a_over_c_major
[C,E,G,A]

> sort (yieldOfChord a_over_c_major)
[A,C,E,G]

> :t Triad (Single C) (Single E) (Single G)
(Triad (Single C) (Single E) (Single G)) :: Chord

> :t yieldOfChord ((Triad (Single C) (Single E) (Single G)))
yieldOfChord ((Triad (Single C) (Single E) (Single G))) :: [Note]

The size of a chord is just the length of its yield.

> (sizeOfChord (Triad (Single C) (Single E) (Single G))) == length (yieldOfChord (Triad (Single C) (Single E) (Single G)))
True

-}

-- Define the head constructor C of a token C a1 ... ar as the token C * ... *.

headToken :: Chord -> Chord
headToken ChordBot = ChordBot
headToken (Single n) = Single n
headToken (Dyad c d) = Dyad ChordBot ChordBot
headToken (Triad c d e) = Triad ChordBot ChordBot ChordBot
headToken (Tetrad c d e f) = Tetrad ChordBot ChordBot ChordBot ChordBot
headToken (Pentad c d e f g) = Pentad ChordBot ChordBot ChordBot ChordBot ChordBot

-- Define the arity of a token; more general, and easier than declaring the arity of a constructor. In particular, what is here defined is the arity of the head constructor of a token.

arity :: Chord -> Int
arity ChordBot              = 0
arity (Single _)            = 0 -- here Single is treated as a nullary constructor!
arity (Dyad _ _)            = 2
arity (Triad _ _ _)         = 3
arity (Tetrad _ _ _ _)      = 4
arity (Pentad _ _ _ _ _)    = 5

-- Define the list of component tokens of a given token.

tokenComponents :: Chord -> [Chord]
tokenComponents ChordBot             = []
tokenComponents (Single _)           = []
tokenComponents (Dyad c d)           = [c, d]
tokenComponents (Triad c d e)        = [c, d, e]
tokenComponents (Tetrad c d e f)     = [c, d, e, f]
tokenComponents (Pentad c d e f g)   = [c, d, e, f, g]

-- Define the list of all subtokens of a given token.

subtokens :: Chord -> [Chord]
subtokens ChordBot              = [ChordBot]
subtokens (Single n)            = [Single n]
subtokens (Dyad c d)            = [Dyad c d] ++ (subtokens c) ++ (subtokens d)
subtokens (Triad c d e)         = [Triad c d e] ++ (subtokens c) ++ (subtokens d) ++ (subtokens e)
subtokens (Tetrad c d e f)      = [Tetrad c d e f] ++ (subtokens c) ++ (subtokens d) ++ (subtokens e) ++ (subtokens f)
subtokens (Pentad c d e f g)    = [Pentad c d e f g] ++ (subtokens c) ++ (subtokens d) ++ (subtokens e) ++ (subtokens f) ++ (subtokens g)


{- EXAMPLES

> componentToken a_over_c_major 1
Just (Triad (Single C) (Single E) (Single G))

> componentToken a_over_c_major 2
Just (Single A)

> componentToken a_over_c_major 3
Nothing

> tokenComponents a_over_c_major
[Triad (Single C) (Single E) (Single G),Single A]

> tokenComponents e_minor_over_c
[Single C,Triad (Single E) (Single G) (Single B)]

> subtokens e_minor_over_c
[Dyad (Single C) (Triad (Single E) (Single G) (Single B)),Single C,Triad (Single E) (Single G) (Single B),Single E,Single G,Single B]

-}

-- Define the usual consistency predicates.

consistent :: Chord -> Chord -> Bool
consistent _ ChordBot   = True
consistent ChordBot _   = True
consistent c d          = headToken c == headToken d && consistentLift (tokenComponents c) (tokenComponents d)

consistentLift :: [Chord] -> [Chord] -> Bool
consistentLift [] []           = True
consistentLift (c:cs) (d:ds)   = consistent c d && consistentLift cs ds
consistentLift _ _             = False

consistentL :: [Chord] -> Bool
consistentL []       = True
consistentL [_]      = True
consistentL (c:d:cs) = consistent c d && consistentL (c:cs)

{- EXAMPLES

> c_major_extended `consistent` a_over_c_major
True

> c_major_extended `consistent` e_minor_over_c
False

> consistentL [c_major_extended, a_over_c_major, e_minor_over_c]
False

> consistentL [c_major_extended, a_over_c_major]
True

> consistentL [Single A, ChordBot, Single B]
False

-}

-- An auxiliary function for the notion of sufficiency: a neighborhood U is sufficient for the constructor C of arity r on the arguments U1, ..., Ur, when for each i = 1, ..., r and each ai in Ui there exists an a in U with head constructor C and i-th component token ai.

sufficient :: [Chord] -> Chord -> [[Chord]] -> Bool -- arg1: U, arg2: C, arg3: U1, 6..., Ur
sufficient u ctr us
    | not (consistentL u) = error "sufficient: inconsistent list"
    | ctr /= headToken ctr = error "sufficient: not a constructor"
    | length us /= r = error "sufficient: too few or too many arguments"
    | otherwise = all (\ i -> all (\ b -> any (\ a -> headToken a == ctr && ((tokenComponents a) !! (i - 1)) == b) u) (us !!(i-1))) indices
    where
        r = arity ctr
        indices = [1..r]

-- An auxiliary function for the constructor application.

ctrapply :: Chord -> [[Chord]] -> [Chord]
ctrapply ctr us
    | ctr /= ctrhead                                                    = error "ctrapply: not a constructor"
--     | any (\ u -> u == []) us                                           = error "ctrapply: empty argument" -- if you include this line the pathform below crashes; in any case, you would need the line in the definition of entails, if you were to use ctrapply there
    | any (\ u -> not (consistentL u)) us                               = error "ctrapply: inconsistent argument"
    | length us /= arity ctr                                            = error "ctrapply: too few or too many arguments"
    | ctrhead == ChordBot                                               = [ChordBot]
    | ctrhead == Single A                                               = [Single A]
    | ctrhead == Single As                                              = [Single As]
    | ctrhead == Single B                                               = [Single B]
    | ctrhead == Single C                                               = [Single C]
    | ctrhead == Single Cs                                              = [Single Cs]
    | ctrhead == Single D                                               = [Single D]
    | ctrhead == Single Ds                                              = [Single Ds]
    | ctrhead == Single E                                               = [Single E]
    | ctrhead == Single F                                               = [Single F]
    | ctrhead == Single Fs                                              = [Single Fs]
    | ctrhead == Single G                                               = [Single G]
    | ctrhead == Single Gs                                              = [Single Gs]
    | ctrhead == Dyad ChordBot ChordBot                                 = [Dyad a b | a <- (us !! 0), b <- (us !! 1)]
    | ctrhead == Triad ChordBot ChordBot ChordBot                       = [Triad a b c | a <- (us !! 0), b <- (us !! 1), c <- (us !! 2)]
    | ctrhead == Tetrad ChordBot ChordBot ChordBot ChordBot             = [Tetrad a b c d | a <- (us !! 0), b <- (us !! 1), c <- (us !! 2), d <- (us !! 3)]
    | ctrhead == Pentad ChordBot ChordBot ChordBot ChordBot ChordBot    = [Pentad a b c d e | a <- (us !! 0), b <- (us !! 1), c <- (us !! 2), d <- (us !! 3), e <- (us !! 4)]
    where
        ctrhead = headToken ctr

-- The following strips a neighborhood of its bottom tokens.

stripBot :: [Chord] -> [Chord]
stripBot u = filter (\ a -> a /= ChordBot) u

-- Define the predicate of entailment; interesting that it works without ctrapply or stripBot -- this needs testing.

entails :: [Chord] -> Chord -> Bool
entails u a
    | not (consistentL u)       = error "inconsistent list"
    | otherwise                 = case a of
                                        ChordBot         -> True
                                        Single n         -> sufficient u (headToken a) [[Single n]]
                                        Dyad b c         -> sufficient u (headToken a) [[b], [c]]
                                        Triad b c d      -> sufficient u (headToken a) [[b], [c], [d]]
                                        Tetrad b c d e   -> sufficient u (headToken a) [[b], [c], [d], [e]]
                                        Pentad b c d e f -> sufficient u (headToken a) [[b], [c], [d], [e], [f]]

-- Finally, define the lift of entails between lists.

entailsL :: [Chord] -> [Chord] -> Bool
entailsL u v = all (\ b -> u `entails` b) v

{- EXAMPLES

> entails [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] (Dyad ChordBot ChordBot)
True

> entails [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] (Dyad e_minor_over_c ChordBot)
True

> entails [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] (Dyad e_minor_over_c (Dyad (Triad (Single C) (Single E) (Single G)) (Single A)))
True

> entails [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] (Dyad e_minor_over_c (Dyad (Triad (Single C) (Single E) (Single G)) ChordBot))
True

> entails [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] (Dyad e_minor_over_c (Dyad (Triad (Single C) (Single E) (Single G)) (Single B)))
False

> [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] `entails` (Dyad e_minor_over_c (Dyad (Triad (Single C) (Single E) (Single G)) (Single B)))
False

> [ChordBot, Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] `entails` (Dyad e_minor_over_c ChordBot)
True

> [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] `entails` (Triad e_minor_over_c ChordBot ChordBot)
False

> [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] `entailsL` [ChordBot,Triad e_minor_over_c ChordBot ChordBot]
False

> [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] `entailsL` [ChordBot,Dyad e_minor_over_c ChordBot]
True

-}

-- Now to some normal forms of neighborhoods. The eigentoken (or supremum) of a neighborhood is defined with the help of an auxiliary function computing suprema of two tokens.

sup :: Chord -> Chord -> Chord
sup ChordBot a = a
sup a ChordBot = a
sup (Single A) (Single A) = (Single A)
sup (Single As) (Single As) = (Single As)
sup (Single B) (Single B) = (Single B)
sup (Single C) (Single C) = (Single C)
sup (Single Cs) (Single Cs) = (Single Cs)
sup (Single D) (Single D) = (Single D)
sup (Single Ds) (Single Ds) = (Single Ds)
sup (Single E) (Single E) = (Single E)
sup (Single F) (Single F) = (Single F)
sup (Single Fs) (Single Fs) = (Single Fs)
sup (Single G) (Single G) = (Single G)
sup (Single Gs) (Single Gs) = (Single Gs)
sup (Dyad a b) (Dyad a' b') = Dyad (sup a a') (sup b b')
sup (Triad a b c) (Triad a' b' c') = Triad (sup a a') (sup b b') (sup c c')
sup (Tetrad a b c d) (Tetrad a' b' c' d') = Tetrad (sup a a') (sup b b') (sup c c') (sup d d')
sup (Pentad a b c d e) (Pentad a' b' c' d' e') = Pentad (sup a a') (sup b b') (sup c c') (sup d d') (sup e e')
sup _ _ = error "sup: inconsistent tokens"

supL :: [Chord] -> Chord
supL [] = ChordBot
supL [a] = a
supL (a:as) = sup a (supL as)

-- The path form of a neighborhood is defined with the help of an auxiliary function computing the path form of a token.

paths :: Chord -> [Chord]
paths ChordBot            = [ChordBot]
paths (Single A)          = [Single A]
paths (Single As)         = [Single As]
paths (Single B)          = [Single B]
paths (Single C)          = [Single C]
paths (Single Cs)         = [Single Cs]
paths (Single D)          = [Single D]
paths (Single Ds)         = [Single Ds]
paths (Single E)          = [Single E]
paths (Single F)          = [Single F]
paths (Single Fs)         = [Single Fs]
paths (Single G)          = [Single G]
paths (Single Gs)         = [Single Gs]
paths (Dyad a b)          = nub $ (ctrapply (Dyad ChordBot ChordBot) [stripBot $ paths a, [ChordBot]]) ++ (ctrapply (Dyad ChordBot ChordBot) [[ChordBot], stripBot $ paths b])
paths (Triad a b c)       = nub $ (ctrapply (Triad ChordBot ChordBot ChordBot) [stripBot $ paths a, [ChordBot], [ChordBot]]) ++ (ctrapply (Triad ChordBot ChordBot ChordBot) [[ChordBot], stripBot $ paths b, [ChordBot]]) ++ (ctrapply (Triad ChordBot ChordBot ChordBot)  [[ChordBot], [ChordBot], stripBot $ paths c])
paths (Tetrad a b c d)    = nub $ (ctrapply (Tetrad ChordBot ChordBot ChordBot ChordBot) [stripBot $ paths a, [ChordBot], [ChordBot], [ChordBot]]) ++ (ctrapply (Tetrad ChordBot ChordBot ChordBot ChordBot) [[ChordBot], stripBot $ paths b, [ChordBot], [ChordBot]]) ++ (ctrapply (Tetrad ChordBot ChordBot ChordBot ChordBot)  [[ChordBot], [ChordBot], stripBot $ paths c, [ChordBot]]) ++ (ctrapply (Tetrad ChordBot ChordBot ChordBot ChordBot) [[ChordBot], [ChordBot], [ChordBot], stripBot $ paths d])
paths (Pentad a b c d e)  = nub $ (ctrapply (Pentad ChordBot ChordBot ChordBot ChordBot ChordBot) [stripBot $ paths a, [ChordBot], [ChordBot], [ChordBot], [ChordBot]]) ++ (ctrapply (Pentad ChordBot ChordBot ChordBot ChordBot ChordBot) [[ChordBot], stripBot $ paths b, [ChordBot], [ChordBot], [ChordBot]]) ++ (ctrapply (Pentad ChordBot ChordBot ChordBot ChordBot ChordBot)  [[ChordBot], [ChordBot], stripBot $ paths c, [ChordBot], [ChordBot]]) ++ (ctrapply (Pentad ChordBot ChordBot ChordBot ChordBot ChordBot) [[ChordBot], [ChordBot], [ChordBot], stripBot $ paths d, [ChordBot]]) ++ (ctrapply (Pentad ChordBot ChordBot ChordBot ChordBot ChordBot) [[ChordBot], [ChordBot], [ChordBot], [ChordBot], stripBot $ paths e])

pathsL :: [Chord] -> [Chord]
pathsL u = paths (supL u)

{- EXAMPLES

> sup (Dyad (Single Cs) (ChordBot)) (Dyad (ChordBot) (Single D))
Dyad (Single Cs) (Single D)

> sup (Dyad (Single Cs) (ChordBot)) (Single D)
*** Exception: sup: inconsistent tokens

> supL [Triad (Single C) ChordBot ChordBot, Triad ChordBot (Single E) ChordBot, Triad ChordBot ChordBot (Single G), ChordBot, Triad (Single C) (Single E) ChordBot]
Triad (Single C) (Single E) (Single G)

> supL []
ChordBot

> supL [Triad (Single C) ChordBot ChordBot, Triad ChordBot (Single E) ChordBot, Triad ChordBot ChordBot (Single G), ChordBot, Triad (Single F) ChordBot ChordBot]
Triad *** Exception: sup: inconsistent tokens

> paths ChordBot
[ChordBot]

> paths (Single A)
[Single A]

> paths (Dyad ChordBot (Single A))
[Dyad ChordBot (Single A)]

> paths (Pentad (Single E) (Single G) (Single B) (Single D) (Pentad (Single E) (Single G) (Single B) (Single D) ChordBot))
[Pentad (Single E) ChordBot ChordBot ChordBot ChordBot,Pentad ChordBot (Single G) ChordBot ChordBot ChordBot,Pentad ChordBot ChordBot (Single B) ChordBot ChordBot,Pentad ChordBot ChordBot ChordBot (Single D) ChordBot,Pentad ChordBot ChordBot ChordBot ChordBot (Pentad (Single E) ChordBot ChordBot ChordBot ChordBot),Pentad ChordBot ChordBot ChordBot ChordBot (Pentad ChordBot (Single G) ChordBot ChordBot ChordBot),Pentad ChordBot ChordBot ChordBot ChordBot (Pentad ChordBot ChordBot (Single B) ChordBot ChordBot),Pentad ChordBot ChordBot ChordBot ChordBot (Pentad ChordBot ChordBot ChordBot (Single D) ChordBot)]

> paths (Dyad (Triad (Single E) (Single G) (Single B)) (Pentad (Single E) (Single G) (Single B) (Single D) ChordBot))
[Dyad (Triad (Single E) ChordBot ChordBot) ChordBot,Dyad (Triad ChordBot (Single G) ChordBot) ChordBot,Dyad (Triad ChordBot ChordBot (Single B)) ChordBot,Dyad ChordBot (Pentad (Single E) ChordBot ChordBot ChordBot ChordBot),Dyad ChordBot (Pentad ChordBot (Single G) ChordBot ChordBot ChordBot),Dyad ChordBot (Pentad ChordBot ChordBot (Single B) ChordBot ChordBot),Dyad ChordBot (Pentad ChordBot ChordBot ChordBot (Single D) ChordBot)]

-}

-- it's high time we had some pretty-printing...

ppn :: Note -> String
ppn n = show n

ppnL :: [Note] -> [String]
ppnL ns = map ppn ns

ppc :: Chord -> String
ppc ChordBot           = "*"
ppc (Single n)         = ppn n
ppc (Dyad c d)         = "(2" ++ (ppc c) ++ (ppc d) ++ ")"
ppc (Triad c d e)      = "(3" ++ (ppc c) ++ (ppc d) ++ (ppc e) ++ ")"
ppc (Tetrad c d e f)   = "(4" ++ (ppc c) ++ (ppc d) ++ (ppc e) ++ (ppc f) ++ ")"
ppc (Pentad c d e f g) = "(5" ++ (ppc c) ++ (ppc d) ++ (ppc e) ++ (ppc f) ++ (ppc g) ++ ")"

ppcL :: [Chord] -> [String]
ppcL as = map ppc as

{- EXAMPLES

> ppcL $ paths ChordBot
["*"]

> ppcL $ paths (Single A)
["A"]

> ppcL $ paths (Dyad ChordBot (Single A))
["(2*A)"]

> ppcL $ paths (Pentad (Single E) (Single G) (Single B) (Single D) (Pentad (Single E) (Single G) (Single B) (Single D) ChordBot))
["(5E****)","(5*G***)","(5**B**)","(5***D*)","(5****(5E****))","(5****(5*G***))","(5****(5**B**))","(5****(5***D*))"]

> ppcL $ paths (Dyad (Triad (Single E) (Single G) (Single B)) (Pentad (Single E) (Single G) (Single B) (Single D) ChordBot))
["(2(3E**)*)","(2(3*G*)*)","(2(3**B)*)","(2*(5E****))","(2*(5*G***))","(2*(5**B**))","(2*(5***D*))"]

> ppc a_over_c_major
"(2(3CEG)A)"

> ppc e_minor_over_c
"(2C(3EGB))"

-}

{---------------------}
{- 3 -- ABOUT RHYTHM -}
{---------------------}

-- We may view time signatures like 4/4, 3/4, et cetera, as pairs of integers.

type TimeSig = (Int,Int) -- well, the integers are supposed to be positive

-- Given a time signature and a number of bars, give all rhythmic patterns for the whole span of bars.

rhythmPatterns :: (Int,Int) -> Int -> [[Bool]]
rhythmPatterns tsig b = [map (\ x -> elem x (numPatterns !! i)) allBeats | i <- [0..(len2-1)]] {-[map (\ x -> elem x (numPatterns !! i)) allBeats | i <- [0..len]]-} {-[ [elem x (numPatterns !! i) | x <- allBeats] | i <- [0..len]]-}
    where
        len = length allBeats
        numPatterns = concat [choose i allBeats | i <- [0..len]]
        allBeats = [1..(b * (fst tsig))]
        len2 = length numPatterns

-- The following yields all rhythm patterns that stress exactly m many beats of all

rhythmPatternsWith :: (Int, Int) -> Int -> Int -> [[Bool]]
rhythmPatternsWith tsig b m = filter ([False | i <- [1..((b*(fst tsig)) - m)]] `isSubsequenceOf`) $ filter ([True | i <- [1..m]] `isSubsequenceOf` ) $ rpats
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

{-------------------------------------}
{- 4 -- TYMOCZKO CHORD CHANGE SPACES -}
{-------------------------------------}

{-

The following draws from [Tymoczko-2006, Tymoczko-2011-book, Tymoczko-2012], and quite substantially from [Hall-Tymoczko-2012]. We'll use n-dimensional Moebius-strip-like spaces, called "orbifolds" in differential geometry, to model harmonies of n-note chord changes, or better, of n-note voice leadings. Let's see how far we can get without using something like the "Data.Modular" module. A basic correspondence to guide the intuition is given by

music - math
chord - point
voice leading - vector
scale - metric

The point of departure is to understand that "n-chords" are simply n-tuples of notes, and chord changes are differences (say, in halfsteps) between the corresponding notes in each voice; to define types to talk about these things wouldn't be a big deal. The interest lies in the notion of distance between two such changes; it is the definition of a distance that would turn each such type into a metric space, but there are good musical reasons to not want to restrict to any particular metric. Instead, we're going to define a partial order in these types, called "submajorization". I think here of the resulting structures as "Tymoczko chord change spaces" or just "Tymoczko spaces". It would be interesting to have a definition of this partial order by induction on n---we'll see.

-}

-- First define the submajorization order in some generality. The predicate "c submaj d" is to be read as "c is submajorized by d", so we have a kind of "less than or equal to".

submaj :: [Int] -> [Int] -> Bool
submaj xs ys
  | length ys == len = foldl (&&) True [(sum $ take i sxs) <= (sum $ take i sys) | i <- [1..len]]
  | otherwise = error "submaj: inputs have unequal length"
  where
    len = length xs
    sxs = sortDesc xs
    sys = sortDesc ys

-- Given two n-voicings v and w, define their displacement set. Note that for this purpose we need zip not with any (symmetric) distance function, but with the minimal change function, since we're interested in voice leadings, that is, in changes between voicings: we have to take into account that a single voice may move higher or lower.

displacement :: [Note] -> [Note] -> [Int]
displacement ns ms
  | length ms == length ns = zipWith minChange ns ms
  | otherwise = error "displacement: inputs have unequal lengths"

{- EXAMPLES -}

{-

> displacement [C, D, G] [C, F, A]
[0,3,2]

> displacement [C, D, G] [Cs, D, G]
[1,0,0]

> displacement [C, E, G] [Cs, E, Fs]
[1,0,-1]

> submaj (displacement [C, D, G] [Cs, D, G]) (displacement [C, D, G] [C, F, A])
True

The following adapts the example from Figure 4 in [Hall-Tymoczko-2012].

> submaj (displacement [C, Ds] [D, F]) (displacement [C, Ds] [F,D])
True

And this one adapts the example from [Hall-Tymoczko-2012, pp. 280, 282], concerning which chord divides the octave more evenly: C major triad or C suspended fourth triad (which is not supported yet above!)? According to our implementation so far we get

> submaj (displacement [C, E, G] [C, E, Gs]) (displacement [C, F, G] [C, E, Gs])
False

and

> submaj (displacement [C, F, G] [C, E, Gs]) (displacement [C, E, G] [C, E, Gs])
True

which is contrary to traditional musical intuition. Obviously, we need the notion of "T-closeness" from the above reference... Now, this will be tricky, since this notion is defined by existential quantification.

-}

{----------------------}
{- 4.1 -- T-CLOSENESS -}
{----------------------}

{-

We follow [Hall-Tymoczko-2012] a bit more closely, in particular, from section 2.1 on.

-}

-- Define the "sigma j" of a list, that is, the sum of the j largest elements of a list (the authors say "multiset").

largestElSum :: Int -> [Int] -> Int
largestElSum j ns = sum $ take j (sortDesc ns)

-- Define the "sigma j ball" of a list [Hall-Tymoczko-2012, Definition 5] as a predicate. Input a list (for the vector), a j, and a list (for the multiset), and output "yes" if the vector is in the sigma j ball of the multiset.

inBall :: [Int] -> Int -> [Int] -> Bool
inBall vs j ns = largestElSum j (map abs vs) <= largestElSum j ns

-- With the help of inBall, define the inner and outer submajorization balls. The inner one is the intersection of all sigma j balls, while the outer one is their union.

inInnerBall :: [Int] -> [Int] -> Bool
inInnerBall vs ns = all (\ j -> inBall vs j ns) [1..(length vs)]

inOuterBall :: [Int] -> [Int] -> Bool
inOuterBall vs ns = any (\ j -> inBall vs j ns) [1..(length vs)]

{- EXAMPLES -}

{-

> inBall [1,-2,3] 1 [2,2,2]
False

> inBall [1,-2,3] 2 [2,2,2]
False

> inBall [1,-2,3] 3 [2,2,2]
True

> inInnerBall [1,-2,3] [2,2,2]
False

> inOuterBall [1,-2,3] [2,2,2]
True

> inInnerBall [1,-1,1] [2,2,2]
True

> inOuterBall [1,-1,1] [2,2,2]
True

-}

-- Now to the concept of T-closeness. Given a change X = {x_1, ..., x_n}, say "absolute change" and write |X| for the change {|x_1|, ..., |x_n|}; also, write X-x for the change {x_1 - x, ..., x_n - x}. A straightforward interpretation of Definition 6 of Hall & Tymoczko, leads us to say that an n-change W is T-smaller than an n-change V if and only if for all 1-changes x there exist 1-changes y such that the absolute change |W - y| is submajorized by the absolute change |V - x|. The immediate questions are: is the carrier of x exhaustible? is the carrier of y searchable? (in the sense of [Escardo-2008]) Well, duh: in our case they can both taken to be finite, so apparently we don't even need the authors' algorithm in Appendix A.2.

tsmaller :: [Int] -> [Int] -> Bool
tsmaller ws vs = all (\ x -> (any (\ y -> submaj (map abs $ map (\ w -> w - y) ws) (map abs $ map (\ v -> v - x) vs)) xyrange)) xyrange
  where
    xyrange = [1..12]

{- EXAMPLES -}

{-

> tsmaller (displacement [C, F, G] [C, E, Gs]) (displacement [C, E, G] [C, E, Gs])
False

-}

-- Nevertheless, it doesn't hurt in principle to have the full generality. This we would need, had we introduced the type Note as a continuous type.

-- Define the middle interval of a list.

middleInterval :: (Fractional t, Ord t) => [t] -> [t]
middleInterval xs = [minimum mset, maximum mset]
  where
    mset = map (\ (m, n) -> (m + n)/2) $ zip supxs sdownxs
    supxs = sort xs
    sdownxs = sortDesc xs

-- Define the set of averages of a given list (the set S of [Hall-Tymoczko-2012, Theorem2]).

averages :: (Eq a, Fractional a) => [a] -> [a]
averages xs = nub $ [(\ (m, n) -> (m + n) / 2) (x, y) | x <- xs, y <- xs]

{------------------------------}
{- 5 -- CONCERNING THE GUITAR -}
{------------------------------}

{-
Watching this guy here, https://www.youtube.com/watch?v=b9pYEjZ4l48, it seemed reasonable to start implementing some of the quirks that are particular to the guitar.
-}

-- Start by introducing two basic datatypes, one for the strings and one for the frets.

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
  putStrLn ("What is the note on fret " ++ show (fromEnum fret) ++ " of string " ++ show (fromEnum string + 1) ++ " in standard tuning?")
  putStrLn " nomenclature: A, As, B, C, Cs, D, ..."
  guess <- getLine
  if guess == show answer then putStr " Correct!" else putStr (" Wrong! The note is actually " ++ show answer ++ ".")

fretboardQuizTwoStandard :: IO ()
fretboardQuizTwoStandard = do
  g1 <- newStdGen
  g2 <- newStdGen
  let note = head (randoms g1 :: [Note])
  let string = head (randoms g2 :: [Guitarstring])
  putStrLn ("Where is the note " ++ show note ++ " on string " ++ show (fromEnum string + 1) ++ " in standard tuning?")
  guess <- readLn --getLine
  if fretboard tuningStandard string (toEnum guess) == note then putStr " Correct!" else putStr " Wrong." -- Needs some more work to also provide the user with the correct answer here.
