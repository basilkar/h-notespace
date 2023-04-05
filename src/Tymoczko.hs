{-------------------------------------}
{- 4 -- TYMOCZKO CHORD CHANGE SPACES -}
{-------------------------------------}

module Tymoczko where

import Note
import Utils
import Data.List
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
  | length ys == len = and [sum (take i sxs) <= sum (take i sys) | i <- [1..len]]
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
tsmaller ws vs = all (\ x -> any (\ y -> submaj (map (abs . (\ w -> w - y)) ws) (map (abs . (\ v -> v - x)) vs)) xyrange) xyrange
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
    mset = zipWith (\ m n -> (m + n) / 2) supxs sdownxs
    supxs = sort xs
    sdownxs = sortDesc xs

-- Define the set of averages of a given list (the set S of [Hall-Tymoczko-2012, Theorem2]).

averages :: (Eq a, Fractional a) => [a] -> [a]
averages xs = nub $ [(\ (m, n) -> (m + n) / 2) (x, y) | x <- xs, y <- xs]
