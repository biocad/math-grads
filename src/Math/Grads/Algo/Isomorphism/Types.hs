{-# LANGUAGE MultiParamTypeClasses #-}
module Math.Grads.Algo.Isomorphism.Types
  ( VertexIndex
  , VComparator
  , EComparator
  , GComparable(..)
  ) where

import           Math.Grads.Graph (Graph, GraphEdge)


-- | Type alias for 'Int'.
--
type VertexIndex = Int

-- | Function that checks whether two vertices are identical.
-- Due to properties related to index of vertex,
-- like number of neighbors, we consider vertex indices instead of vertices.
--
type VComparator v1 v2 = VertexIndex -> VertexIndex -> Bool

-- | Function that checks whether two edges are identical.
-- Due to properties related to index of vertex,
-- like belonging to a cycle, we consider GraphEdge (Int, Int, e) instead of e.
--
type EComparator e1 e2 = GraphEdge e1 -> GraphEdge e2 -> Bool

-- | Type class for graphs that could be checked for isomorphism.
--
class (Graph g1, Graph g2) => GComparable g1 v1 e1 g2 v2 e2 where
  vComparator :: g1 v1 e1 -> g2 v2 e2 -> VComparator v1 v2
  eComparator :: g1 v1 e1 -> g2 v2 e2 -> EComparator e1 e2
