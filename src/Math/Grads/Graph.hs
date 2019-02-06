-- | Module that provides 'Graph' type class and several useful functions
-- for interaction with 'Graph's.
--
module Math.Grads.Graph
  ( EdgeList
  , Graph (..)
  , GraphEdge
  , changeIndsEdge
  , changeTypeEdge
  , edgeType
  ) where

import           Data.List (nub)

-- | 'GraphEdge' is just triple, containing index of starting vertex of edge,
-- index of ending vertex of edge and edge's type.
--
type GraphEdge e = (Int, Int, e)

-- | Type alias for list of 'GraphEdge's.
--
type EdgeList e = [GraphEdge e]

-- | Get edge's type from 'GraphEdge'.
--
edgeType :: GraphEdge e -> e
edgeType (_, _, t) = t

-- | Given transformation of edge types transforms 'GraphEdge'.
--
changeTypeEdge :: (e1 -> e2) -> GraphEdge e1 -> GraphEdge e2
changeTypeEdge f (a, b, t) = (a, b, f t)

-- | Given transformation of edge's indices transforms 'GraphEdge'.
--
changeIndsEdge :: (Int -> Int) -> GraphEdge e -> GraphEdge e
changeIndsEdge f (a, b, t) = (f a, f b, t)

-- | Type class that gives data structure properties of graph.
--
class Graph g where
  -- | Construct a graph from list of vertices and edges.
  --
  fromList :: (Ord v, Eq v) => ([v], [GraphEdge e]) -> g v e

  -- | Get a list of all vertices and edges from the graph.
  --
  toList :: (Ord v, Eq v) => g v e -> ([v], [GraphEdge e])

  -- | Get the number of vertices.
  --
  vCount :: g v e -> Int

  -- | Unsafe get adjacent vertices.
  --
  infixl 9 !>
  (!>) :: (Ord v, Eq v) => g v e -> v -> [(v, e)]

  -- | Unsafe get adjacent indices.
  --
  infixl 9 !.
  (!.) :: g v e -> Int -> [(Int, e)]

  -- | Safe get adjacent vertices.
  --
  infixl 9 ?>
  (?>) :: (Ord v, Eq v) => g v e -> v -> Maybe [(v, e)]

  -- | Safe get adjacent indices.
  --
  infixl 9 ?.
  (?.) :: g v e -> Int -> Maybe [(Int, e)]

  -- | Get a list of edges starting at given vertex.
  --
  incident :: (Ord v, Eq v) => g v e -> v -> [(v, v, e)]
  incident gr at = (\(a, b) -> (at, a, b)) <$> gr !> at

  -- | Safe get a list of edges starting at given vertex.
  --
  safeIncident :: (Ord v, Eq v) => g v e -> v -> Maybe [(v, v, e)]
  safeIncident gr at = map (\(a, b) -> (at, a, b)) <$> gr ?> at

  -- | Get a list of index edges starting at given vertex.
  --
  incidentIdx :: (Eq e) => g v e -> Int -> [GraphEdge e]
  incidentIdx gr idx = nub ((\(a, b) -> (min idx a, max idx a, b)) <$> gr !. idx)

  -- | Safe get a list of index edges starting at given vertex.
  --
  safeIncidentIdx :: (Eq e) => g v e -> Int -> Maybe [GraphEdge e]
  safeIncidentIdx gr idx = nub <$> (map (\(a, b) -> (min idx a, max idx a, b)) <$> gr ?. idx)
