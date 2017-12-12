module Math.Grads.Utils
  ( nub
  , subsets
  , uniter
  ) where

import           Data.List (group, sort)

nub :: (Ord a, Eq a) => [a] -> [a]
nub = fmap head . group . sort

uniter :: [a] -> [(a, a)]
uniter l = zip l $ drop 1 l

subsets :: [a] -> [[a]]
subsets []       = [[]]
subsets (x : xs) = subsets xs ++ ((x :) <$> subsets xs)
