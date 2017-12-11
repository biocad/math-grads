{-# LANGUAGE InstanceSigs #-}

module Math.Graph.GenericGraph
  ( GenericGraph (..)
  , addVertices
  , addEdges
  , applyG
  , applyV
  , getVertices
  , getEdge
  , removeEdges
  , removeVertices
  , safeAt
  , safeIdx
  , subgraph
  , sumGraphs
  , typeOfEdge
  ) where

import           Control.Arrow    (first)
import qualified Data.Array       as A
import           Data.List        (find, groupBy, sortBy)
import           Data.Map.Strict  (Map, mapKeys, member, (!))
import qualified Data.Map.Strict  as M
import           Data.Maybe       (fromJust, fromMaybe)
import qualified Data.Set         as S

import           Math.Graph.Class (Graph (..))

data GenericGraph v e = GenericGraph {
    gIndex     :: A.Array Int v,
    gRevIndex  :: Map v Int,
    gAdjacency :: A.Array Int [(Int, e)]
}

instance Graph GenericGraph where
  fromList :: (Ord v, Eq v) => ([v], [(Int, Int, e)]) -> GenericGraph v e
  fromList (vertices, edges) = GenericGraph idxArr revMap adjArr
    where
      count = length vertices
      idxArr = A.listArray (0, count - 1) vertices
      revMap = M.fromList $ zip vertices [0..]
      indices = concatMap insertFunc edges
      insertFunc (at, other, b) | at == other = [(at, (other, b))]
                                | otherwise = [(at, (other, b)), (other, (at, b))]
      adjArr = A.accumArray (flip (:)) [] (0, count - 1) indices

  toList :: (Ord v, Eq v) => GenericGraph v e -> ([v], [(Int, Int, e)])
  toList (GenericGraph idxArr _ adjArr) = (snd <$> A.assocs idxArr, edges)
    where
      edges = distinct . concatMap toEdges . A.assocs $ adjArr
      toEdges (k, v) = map (toAscending k) v
      toAscending k (a, b) | k > a = (a, k, b)
                           | otherwise = (k, a, b)
      compare3 (at1, other1, _) (at2, other2, _) = compare (at1, other1) (at2, other2)
      eq3 v1 v2 = compare3 v1 v2 == EQ
      distinct = map head . groupBy eq3 . sortBy compare3

  vCount :: GenericGraph v e -> Int
  vCount (GenericGraph idxArr _ _) = length idxArr

  (!>) :: (Ord v, Eq v) => GenericGraph v e -> v -> [(v, e)]
  (GenericGraph idxArr revMap adjArr) !> at = first (idxArr A.!) <$> adjacent
    where
      idx = revMap ! at
      adjacent = adjArr A.! idx

  (?>) :: (Ord v, Eq v) => GenericGraph v e -> v -> Maybe [(v, e)]
  gr@(GenericGraph _ revMap _) ?> at | at `member` revMap = Just (gr !> at)
                                     | otherwise = Nothing


  (!.) :: GenericGraph v e -> Int -> [(Int, e)]
  (!.) (GenericGraph _ _ adjArr) = (adjArr A.!)

  (?.) :: GenericGraph v e -> Int -> Maybe [(Int, e)]
  gr@(GenericGraph _ _ adjArr) ?. idx | idx `inBounds` A.bounds adjArr = Just (gr !. idx)
                                      | otherwise = Nothing


instance (Ord v, Eq v, Show v, Show e) => Show (GenericGraph v e) where
  show gr = unlines . map fancyShow . filter (\(a, b, _) -> a < b) . snd . toList $ gr
    where
      idxArr = gIndex gr
      fancyShow (at, other, bond) = concat [show $ idxArr A.! at, "\t", show bond, "\t", show $ idxArr A.! other]

instance Functor (GenericGraph v) where
  fmap f (GenericGraph idxArr revMap adjArr) = GenericGraph idxArr revMap (((f <$>) <$>) <$> adjArr)

-- Check whether or not given value is betwen bonds.
inBounds :: Ord a => a -> (a, a) -> Bool
inBounds idx (lo, hi) = (idx >= lo) && (idx <= hi)


-- 'fmap' which acts on adjacency lists of each vertex
applyG :: ([(Int, e1)] -> [(Int, e2)]) -> GenericGraph v e1 -> GenericGraph v e2
applyG f (GenericGraph idxArr revMap adjArr) = GenericGraph idxArr revMap (f <$> adjArr)

-- 'fmap' which acts on vertices
applyV :: Ord v2 => (v1 -> v2) -> GenericGraph v1 e -> GenericGraph v2 e
applyV f (GenericGraph idxArr revMap adjArr) = GenericGraph (f <$> idxArr) (mapKeys f revMap) adjArr

-- All vertices of the graph.
getVertices :: GenericGraph v e -> [v]
getVertices (GenericGraph idxArr _ _) = map snd $ A.assocs idxArr

-- Get subgraph on given vertices. Note that indexation will be CHANGED. Be careful with !. and ?. operators.
subgraph :: Ord v => GenericGraph v e -> [Int] -> GenericGraph v e
subgraph graph toKeep = fromList (newVertices, newEdges)
  where
    vSet :: S.Set Int
    vSet = S.fromList toKeep

    eRemain :: (Int, Int, e) -> Bool
    eRemain (at, other, _) = (at `S.member` vSet) && (other `S.member` vSet)

    (oldVertices, edges) = filter eRemain <$> toList graph
    (newVertices, oldIdx) = unzip . filter (\(_, ix) -> ix `S.member` vSet) $ zip oldVertices [0..]

    vMap :: Map Int Int
    vMap = M.fromList $ zip oldIdx [0 ..]

    newEdges = map (\(at, other, bond) -> (vMap ! at, vMap ! other, bond)) edges

addVertices :: Ord v => GenericGraph v e -> [v] -> GenericGraph v e
addVertices graph toAdd = fromList ((\(x, y) -> (x ++ toAdd, y)) (toList graph))

-- Remove given vertices from the graph. Note that indexation will be CHANGED. Be careful with !. and ?. operators.
removeVertices :: Ord v => GenericGraph v e -> [Int] -> GenericGraph v e
removeVertices graph toRemove = fromList (newVertices, newEdges)
  where
    vSet :: S.Set Int
    vSet = S.fromList toRemove

    eRemove :: (Int, Int, e) -> Bool
    eRemove (at, other, _) = (at `S.notMember` vSet) && (other `S.notMember` vSet)

    (oldVertices, edges) = filter eRemove <$> toList graph
    (newVertices, oldIdx) = unzip . filter (\(_, ix) -> ix `S.notMember` vSet) $ zip oldVertices [0..]

    vMap :: Map Int Int
    vMap = M.fromList $ zip oldIdx [0 ..]

    newEdges = map (\(at, other, bond) -> (vMap ! at, vMap ! other, bond)) edges

-- Remove given edges from the graph. Note that isolated vertices are allowed. This will NOT affect indexation.
removeEdges :: Ord v => GenericGraph v e -> [(Int, Int)] -> GenericGraph v e
removeEdges graph toRemove = fromList (vertices, edges)
  where
    eSet :: S.Set (Int, Int)
    eSet = S.fromList toRemove

    (vertices, edges) = filter eRemove <$> toList graph

    eRemove (at, other, _) = ((at, other) `S.notMember` eSet) && ((other, at) `S.notMember` eSet)

-- Add given edges to the graph.
addEdges :: Ord v => GenericGraph v e -> [(Int, Int, e)] -> GenericGraph v e
addEdges (GenericGraph inds rinds edges) toAdd = GenericGraph inds rinds res
  where
    accumList = foldl (\x (a, b, t) -> x ++ [(a, (b, t)), (b, (a, t))]) [] toAdd
    res = A.accum (flip (:)) edges accumList

typeOfEdge :: Ord v => GenericGraph v e -> Int -> Int -> e
typeOfEdge graph fromInd toInd = res
  where
    neighbors = gAdjacency graph A.! fromInd
    res = snd (fromJust (find ((== toInd) . fst) neighbors))

-- Safe extraction from the graph. If there is no requested key in it, empty list is returned.
safeIdx :: GenericGraph v e -> Int -> [Int]
safeIdx graph = map fst . fromMaybe [] . (graph ?.)

safeAt :: GenericGraph v e -> Int -> [(Int, e)]
safeAt graph = fromMaybe [] . (graph ?.)

getEdge :: GenericGraph v e -> Int -> Int -> e
getEdge graph from to = found
  where
    neighbors = graph !. from
    found = snd (fromJust (find ((== to) . fst) neighbors))

sumGraphs :: Ord v => GenericGraph v e -> GenericGraph v e -> GenericGraph v e
sumGraphs molGraphA molGraphB = res
  where
    (vertA, edgeA) = toList molGraphA
    (vertB, edgeB) = toList molGraphB
    renameMapB = M.fromList (zip [0..length vertB - 1] [length vertA..length vertA + length vertB - 1])
    renameFunc = (renameMapB M.!)

    newVertices = vertA ++ vertB
    newEdges = edgeA ++ fmap (\(a, b, t) -> (renameFunc a, renameFunc b, t)) edgeB

    res = fromList (newVertices, newEdges)
