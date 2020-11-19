module Utils where

import Data.Array -- for the memoization in the definition of Levenshtein metric
import Data.List

-- Define the Levenshtein metric as given on https://www.reddit.com/r/programming/comments/w4gs6/levenshtein_distance_in_haskell/c5a6jjz/ . It is polymorphic, so readily applicable to our case, in contrast to the existing package Text.EditDistance.
levenshtein :: (Eq a) => [a] -> [a] -> Int
levenshtein xs ys = memoArray ! (n, m)
  where memoArray = array ((0,0),(n,m)) [((i,j),levAux i j) | i <- [0..n], j <- [0..m]]
        n = length xs
        m = length ys
        xa = listArray (1, n) xs
        ya = listArray (1, m) ys
        levAux 0 v = v
        levAux u 0 = u
        levAux u v
          | xa ! u == ya ! v = memoArray ! (u-1, v-1)
          | otherwise        = 1 + minimum [memoArray ! (u, v-1),
                                            memoArray ! (u-1, v),
                                            memoArray ! (u-1, v-1)]

-- Define a descending sorting function (check out https://ro-che.info/articles/2016-04-02-descending-sort-haskell), to be used at the definition of submajorization.

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)