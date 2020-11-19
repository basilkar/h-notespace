{--------------------------------------------------}
{- 2 -- AN INDUCTIVE TYPE FOR CHORD CONSTRUCTIONS -}
{--------------------------------------------------}

module Chord where

import Note
import Data.List

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
