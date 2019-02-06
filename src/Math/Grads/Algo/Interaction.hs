-- | Module that provides various functions for interaction with 'GraphEdge's,
-- 'EdgeList's and vertices themselves.
--
module Math.Grads.Algo.Interaction
  (
    -- * Vertex Functions
    --
    areAdjacent
  , getEnds
  , getOtherEnd
  , getSharedVertex
  , getVertexAdjacent
  , getVertexIncident
  , getVertexIncidentIdx
  , haveSharedVertex
  , isIncident
  , (~=)
  , (/~=)
    -- * Edge Functions
    --
  , matchEdges
  , getEdgeIncident
    -- * EdgeList Functions
    --
  , doubleEdgeList
  , edgeListToMap
  , haveSharedEdge
  , sortBondList
  , getIndices
  ) where

import           Data.List        (find, findIndices, intersect, sortOn)
import           Data.Map         (Map)
import qualified Data.Map         as M
import           Data.Maybe       (fromJust, isJust)

import           Math.Grads.Graph (EdgeList, GraphEdge, edgeType)
import           Math.Grads.Utils (nub)

-- | Equality operator for 'GraphEdge's.
--
(~=) :: GraphEdge e1 -> GraphEdge e2 -> Bool
(b, e, _) ~= (b', e', _) = (b == b' && e == e') || (b == e' && e == b')

-- | Inequality operator for 'GraphEdge's.
--
(/~=) :: GraphEdge e1 -> GraphEdge e2 -> Bool
b1 /~= b2 = not $ b1 ~= b2

-- | Checks that vertex with given index is incident to 'GraphEdge'.
--
isIncident :: GraphEdge e -> Int -> Bool
isIncident (b, e, _) n = b == n || e == n

-- | Find all edges in given 'EdgeList' that are incident to vertex with given index.
--
getVertexIncident :: EdgeList e -> Int -> EdgeList e
getVertexIncident bs n = filter (`isIncident` n) bs

-- | Returns indices of edges in 'EdgeList' that are incident to vertex with given index.
--
getVertexIncidentIdx :: EdgeList e -> Int -> [Int]
getVertexIncidentIdx bs n = findIndices (`isIncident` n) bs

-- | Returns index of vertex incident to given 'GraphEdge' and different from passed index.
--
getOtherEnd :: GraphEdge e -> Int -> Int
getOtherEnd (b, e, _) n | b == n = e
                        | e == n = b
                        | otherwise = error "There is no such index in edge."

-- | Finds in given 'EdgeList' all indices of vertices adjacent to given vertex.
--
getVertexAdjacent :: EdgeList e -> Int -> [Int]
getVertexAdjacent bs n = (`getOtherEnd` n) <$> getVertexIncident bs n

-- | Checks whether two vertices with given indices are adjacent in given 'EdgeList'.
--
areAdjacent :: EdgeList e -> Int -> Int -> Bool
areAdjacent bs n n' = n' `elem` getVertexAdjacent bs n

-- | Retrieves indices of vertices that are being connected by given 'GraphEdge'.
--
getEnds :: GraphEdge e -> [Int]
getEnds (b, e, _) = [b, e]

-- | Checks that two edges have common vertex.
--
haveSharedVertex :: GraphEdge e1 -> GraphEdge e2 -> Bool
haveSharedVertex b1 b2 = isJust $ getSharedVertex b1 b2

-- | Gets shared common vertex of two edges. If edges don't have common vertex,
-- returns Nothing.
--
getSharedVertex :: GraphEdge e1 -> GraphEdge e2 -> Maybe Int
getSharedVertex b1 b2 | null is        = Nothing
                      | length is == 2 = Nothing
                      | otherwise      = Just $ head is
  where
    is = getEnds b1 `intersect` getEnds b2

-- | Find edges in 'EdgeList' which ordered pairs of indices, that they are connecting,
-- are present in passed list of ordered pairs.
--
matchEdges :: EdgeList e -> [(Int, Int)] -> EdgeList e
matchEdges bonds = fmap (\(a, b) -> fromJust (find (~= (a, b, undefined)) bonds))

-- | Find all edges that are incident to edge in 'EdgeList' with given index.
--
getEdgeIncident :: Ord e => EdgeList e -> Int -> EdgeList e
getEdgeIncident bs n | n >= length bs = []
                     | otherwise = filter (/= (beg, end, typ)) $ getVertexIncident bs beg ++ getVertexIncident bs end
  where
    (beg, end, typ) = bs !! n

-- | For every edge in 'EdgeList' add to that list an edge in opposite direction.
--
doubleEdgeList :: EdgeList e -> EdgeList e
doubleEdgeList = concatMap (\(a, b, t) -> [(a, b, t), (b, a, t)])

-- | Transforms 'EdgeList' into 'Map' that corresponds to adjacency list of undirected
-- graph induced by these edges.
--
edgeListToMap :: EdgeList e -> Map Int [Int]
edgeListToMap bonds' = M.fromList (fmap (toVertex bonds) (getIndices bonds))
  where
    bonds = doubleEdgeList bonds'

    toVertex :: EdgeList e -> Int -> (Int, [Int])
    toVertex bs i = (i, concatMap (\(a, b, _) -> [a | b == i]) bs)

-- | Checks that two 'EdgeList's have common edge.
--
haveSharedEdge :: Eq e => EdgeList e -> EdgeList e -> Bool
haveSharedEdge b1 b2 = or $ fmap (`elem` b2) b1

-- | Sorting for 'EdgeList', that sorts edges on their type, then on index of their
-- to (right) vertex, then on index of their from (left) vertex.
--
sortBondList :: Ord e => EdgeList e -> EdgeList e
sortBondList = sortOn left . sortOn right . sortOn edgeType
  where
    left (a, _, _) = a
    right (_, b, _) = b

-- | Gets all vertices from 'EdgeList'.
--
getIndices :: EdgeList e -> [Int]
getIndices = nub . concatMap getEnds
