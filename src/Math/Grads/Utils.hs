-- | Different utility functions for usage in Math.Grads.
--
module Math.Grads.Utils
  ( nub
  , subsets
  , uniter
  ) where

import           Data.List (group, sort)

-- | nub that works in O(n log n) time.
--
nub :: (Ord a, Eq a) => [a] -> [a]
nub = fmap head . group . sort

-- | Zips list with its tail.
--
uniter :: [a] -> [(a, a)]
uniter [] = []
uniter l  = zip l $ drop 1 l

-- | Returns all possible subsets of given list as list of lists.
--
subsets :: [a] -> [[a]]
subsets []       = [[]]
subsets (x : xs) = subsets xs ++ ((x :) <$> subsets xs)
