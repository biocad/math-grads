{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Module that provides functions for identifying graph/subgraph isomorphism.
--
module Math.Grads.Algo.Isomorphism
  ( EComparator, VComparator
  , GComparable(..)
  , VertexIndex
  , getIso
  , getMultiIso
  , isIso
  , isIsoSub
  ) where

import           Data.Map                          (Map)
import           Data.Maybe                        (isJust, listToMaybe)

import qualified Math.Grads.Algo.Isomorphism.RI    as RI
import           Math.Grads.Algo.Isomorphism.Types (EComparator,
                                                    GComparable (..),
                                                    VComparator, VertexIndex)
import           Math.Grads.GenericGraph           (GenericGraph (..))
import           Math.Grads.Graph                  (toList)


-- | Checks whether two graphs are isomorphic.
--
isIso :: (Ord v1, Ord v2, GComparable GenericGraph v1 e1 GenericGraph v2 e2, Eq e1, Eq e2)
      => GenericGraph v1 e1
      -> GenericGraph v2 e2
      -> Bool
isIso queryGraph targetGraph = res
  where
    (v1, e1) = toList queryGraph
    (v2, e2) = toList targetGraph
    isoSub = isIsoSub queryGraph targetGraph

    res = length v1 == length v2 && length e1 == length e2 && isoSub

-- | Check for queryGraph \( \subseteq \) targetGraph.
--
isIsoSub :: (Ord v1, Ord v2, GComparable GenericGraph v1 e1 GenericGraph v2 e2, Eq e1, Eq e2)
         => GenericGraph v1 e1 -- ^ queryGraph
         -> GenericGraph v2 e2 -- ^ targetGraph
         -> Bool
isIsoSub queryGraph targetGraph = isJust $ getIso queryGraph targetGraph

-- | Get one vertices matching (if exists) from queryGraph to targetGraph.
--
getIso :: (Ord v1, Ord v2, GComparable GenericGraph v1 e1 GenericGraph v2 e2, Eq e1, Eq e2)
       => GenericGraph v1 e1 -- ^ queryGraph
       -> GenericGraph v2 e2 -- ^ targetGraph
       -> Maybe (Map Int Int)
getIso queryGraph targetGraph = listToMaybe $ getMultiIso queryGraph targetGraph

-- | Get all possible vertices matchings from queryGraph to targetGraph.
--
getMultiIso :: (Ord v1, Ord v2, GComparable GenericGraph v1 e1 GenericGraph v2 e2, Eq e1, Eq e2)
            => GenericGraph v1 e1 -- ^ queryGraph
            -> GenericGraph v2 e2 -- ^ targetGraph
            -> [Map Int Int]
getMultiIso = RI.getMultiIso
