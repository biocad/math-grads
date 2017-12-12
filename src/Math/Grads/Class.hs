module Math.Grads.Class
  ( EdgeList
  , Graph (..)
  , GraphEdge
  , edgeType
  ) where

import           Data.List (nub)

type GraphEdge e = (Int, Int, e)
type EdgeList e = [GraphEdge e]

edgeType :: GraphEdge e -> e
edgeType (_, _, t) = t

class Graph g where
  -- Construct a graph from list of vertices and edges
  fromList :: (Ord v, Eq v) => ([v], [GraphEdge e]) -> g v e

  -- Get a list of all vertices and edges from the graph
  toList :: (Ord v, Eq v) => g v e -> ([v], [GraphEdge e])

  -- Get the number of vertices
  vCount :: g v e -> Int

  infixl 9 !> -- Unsafe get adjacent vertices
  (!>) :: (Ord v, Eq v) => g v e -> v -> [(v, e)]

  infixl 9 !. -- Unsafe get adjacent indices
  (!.) :: g v e -> Int -> [(Int, e)]

  infixl 9 ?> -- Safe get adjacent vertices
  (?>) :: (Ord v, Eq v) => g v e -> v -> Maybe [(v, e)]

  infixl 9 ?. -- Safe get adjacent indices
  (?.) :: g v e -> Int -> Maybe [(Int, e)]

  -- Get a list of edges starting at given vertex
  incident :: (Ord v, Eq v) => g v e -> v -> [(v, v, e)]
  incident gr at = (\(a, b) -> (at, a, b)) <$> gr !> at

  -- Safe get a list of edges starting at given vertex
  safeIncident :: (Ord v, Eq v) => g v e -> v -> Maybe [(v, v, e)]
  safeIncident gr at = map (\(a, b) -> (at, a, b)) <$> gr ?> at

  -- Get a list of index edges starting at given vertex
  incidentIdx :: (Eq e) => g v e -> Int -> [GraphEdge e]
  incidentIdx gr idx = nub ((\(a, b) -> (min idx a, max idx a, b)) <$> gr !. idx)

  -- Safe get a list of index edges starting at given vertex
  safeIncidentIdx :: (Eq e) => g v e -> Int -> Maybe [GraphEdge e]
  safeIncidentIdx gr idx = nub <$> (map (\(a, b) -> (min idx a, max idx a, b)) <$> gr ?. idx)

-- Generic undirected graph which stores elements of type 'a' in its vertices (e.g. labels, atoms, states etc)
-- and elements of type 'b' in its edges (e.g. weights, bond types, functions over states etc).
-- Note that loops and multiple edges between two vertices are allowed.
-- TODO: try to replace map with O(1) random access container
-- TODO: try to get rid of the (Eq a, Ord a) constraints
-- TODO: write kind of specialization for GenericGraph Int b, because gIndex and gRevIndex are just id in this case.
--       You may use (!.) and (?.) operators and indices and safeIndices functions with GenericGraph Int b instead.
