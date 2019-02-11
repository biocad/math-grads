{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Module that provides functions for identifying graph/subgraph isomorphism.
--
module Math.Grads.Algo.Isomorphism
  ( EComparator, VComparator
  , GComparable (..)
  , VertexIndex
  , getIso
  , getMultiIso
  , isIso
  , isIsoSub
  ) where

import           Control.Arrow           (second, (&&&), (***))
import qualified Data.Array              as A
import           Data.List               (delete, sortOn)
import           Data.Map                (Map)
import qualified Data.Map.Strict         as M
import           Data.Matrix             (Matrix (..), getElem, getRow, mapRow,
                                          matrix, multStd, ncols, nrows,
                                          setElem, transpose)
import           Data.Maybe              (isJust, listToMaybe)
import           Data.Tuple              (swap)
import qualified Data.Vector             as V
import           Math.Grads.GenericGraph (GenericGraph (..))
import           Math.Grads.Graph        (Graph, GraphEdge, changeIndsEdge,
                                          fromList, incidentIdx, toList, vCount,
                                          (!.))
import           Math.Grads.Utils        (nub)

type GenericGraphIso v e = GenericGraph Int e

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
getMultiIso queryGraph' targetGraph' = matches
  where
    ((queryGraph, queryGraphWI), fromIsoToOldQ) = second inverseMap $ graphToGraphIso queryGraph'
    ((targetGraph, targetGraphWI), fromIsoToOldT) = second inverseMap $ graphToGraphIso targetGraph'

    vComp = vComparator queryGraphWI targetGraphWI
    eComp = eComparator queryGraphWI targetGraphWI

    isos = isoGraph vComp eComp queryGraph targetGraph
    matches = fmap (\x -> getMatchMap x fromIsoToOldQ fromIsoToOldT) isos

inverseMap :: Map Int Int -> Map Int Int
inverseMap = M.fromList . (swap <$>) . M.toList

getMatchMap :: Matrix Int -> Map Int Int -> Map Int Int -> Map Int Int
getMatchMap isoMatrix fromIsoToOldQ fromIsoToOldT = res
  where
    forMap = fmap (getMatchRow isoMatrix) [0 .. nrows isoMatrix - 1]
    res = M.fromList (fmap ((fromIsoToOldQ M.!) *** (fromIsoToOldT M.!)) forMap)

getMatchRow :: Matrix Int -> Int -> (Int, Int)
getMatchRow isoMatrix ind = (ind, helper 0)
  where
    row = getRow (ind + 1) isoMatrix

    helper :: Int -> Int
    helper counter = if row V.! counter == 1 then counter
                     else helper (counter + 1)

isoGraph :: (Eq e1, Eq e2) => VComparator v1 v2
                           -> EComparator e1 e2
                           -> GenericGraphIso v1 e1
                           -> GenericGraphIso v2 e2
                           -> [Matrix Int]
isoGraph vComp eComp queryGraph targetGraph = res
  where
    queryGraphEdges = (fst <$>) <$> gAdjacency queryGraph
    sizeOfQueryGraph = vCount queryGraph
    pMatrix = matrix sizeOfQueryGraph sizeOfQueryGraph (\(i, j) -> if i - 1 `elem` queryGraphEdges A.! (j - 1) then 1 else 0)

    targetGraphEdges = (fst <$>) <$> gAdjacency targetGraph
    sizeOfTargetGraph = vCount targetGraph
    gMatrix = matrix sizeOfTargetGraph sizeOfTargetGraph (\(i, j) -> if i - 1 `elem` targetGraphEdges A.! (j - 1) then 1 else 0)

    mMatrix = matrix sizeOfQueryGraph sizeOfTargetGraph (\(i, j) -> if fits vComp eComp queryGraph targetGraph i j then 1 else 0)

    currentRow = 0
    unusedColumns = [1 .. ncols mMatrix]

    res = recurse eComp queryGraph targetGraph unusedColumns currentRow gMatrix pMatrix mMatrix

fits :: (Eq e1, Eq e2) => VComparator v1 v2
                       -> EComparator e1 e2
                       -> GenericGraphIso v1 e1
                       -> GenericGraphIso v2 e2
                       -> Int
                       -> Int
                       -> Bool
fits vComp eComp queryGraph targetGraph i j = res
  where
    (vertex, edges) = (gIndex queryGraph A.! (i - 1), incidentIdx queryGraph $ i - 1)
    (vertex', edges') = (gIndex targetGraph A.! (j - 1), incidentIdx targetGraph $ j - 1)
    res = length edges <= length edges' && canBeSubset eComp edges edges' && vertex `vComp` vertex'

canBeSubset :: forall e1 e2. EComparator e1 e2 -> [GraphEdge e1] -> [GraphEdge e2] -> Bool
canBeSubset eComp query target = uniqueSeq maps
  where
    bondsInd = zip [0 ..] target
    maps = findMatches <$> query

    findMatches :: GraphEdge e1 -> [Int]
    findMatches thisEdge = fst <$> filter (\(_, otherEdge) -> eComp thisEdge otherEdge) bondsInd

uniqueSeq :: [[Int]] -> Bool
uniqueSeq maps = res
  where
    seqs = sequence maps

    res = any (\x -> length x == length (nub x)) seqs

-- | Converts input graph into graph in which vertices with most amount of edges have lowest indices.
--
graphToGraphIso :: (Ord v) => GenericGraph v e
                           -> ((GenericGraphIso v e, GenericGraph v e), M.Map Int Int)
graphToGraphIso graph = res
  where
    (vertices, edges) = toList graph
    vArr = gIndex graph

    indsWithNCount = fmap (id &&& (length . (graph !.))) [0 .. length vertices - 1]
    sortedInds = fst <$> sortOn (\x -> - (snd x)) indsWithNCount
    changesMap = M.fromList (zip sortedInds [0 ..])

    sortedV = fmap (vArr A.!) sortedInds
    changedEdges = fmap (changeIndsEdge (changesMap M.!)) edges

    forGraphWI = (sortedV, changedEdges)
    forGraph = ([0 .. length sortedV - 1], changedEdges)

    res = ((fromList forGraph, fromList forGraphWI), changesMap)

-- | Ullmann's subgraph isomorphism algorithm itself.
--
recurse :: (Eq e1, Eq e2) => EComparator e1 e2
                          -> GenericGraphIso v1 e1
                          -> GenericGraphIso v2 e2
                          -> [Int]
                          -> Int
                          -> Matrix Int
                          -> Matrix Int
                          -> Matrix Int
                          -> [Matrix Int]
recurse eComp queryGraph targetGraph unusedColumns currentRow gMatrix pMatrix mMatrix = res
  where
    prunedM = prune eComp queryGraph targetGraph mMatrix currentRow

    recs = concatMap pruneNext unusedColumns

    res | hasEmptyRow mMatrix = []
        | currentRow == nrows mMatrix && isIsomorphism gMatrix pMatrix mMatrix = [mMatrix]
        | not (hasEmptyRow prunedM) = recs
        | otherwise = []

    pruneNext :: Int -> [Matrix Int]
    pruneNext x = recurse eComp queryGraph targetGraph newColumns newRow gMatrix pMatrix changedMatrix
      where
        newColumns = delete x unusedColumns
        newRow = currentRow + 1
        changedMatrix = changeRow prunedM newRow x

prune :: (Eq e1, Eq e2) => EComparator e1 e2
                        -> GenericGraphIso v1 e1
                        -> GenericGraphIso v2 e2
                        -> Matrix Int
                        -> Int
                        -> Matrix Int
prune eComp queryGraph targetGraph mMatrix currentRow | null indicesToChange = mMatrix
                                                      | hasEmptyRow mMatrix = mMatrix
                                                      | otherwise = res
  where
    numberOfMRows = nrows mMatrix
    numberOfMColumns = ncols mMatrix
    pairsOfindices = [(i, j) | i <- [1.. numberOfMRows], j <- [1.. numberOfMColumns], getElem i j mMatrix == 1]

    suitPair :: Int -> Int -> Bool
    suitPair = hasSuitableNeighbors eComp queryGraph targetGraph mMatrix

    indicesToChange = filter (not . uncurry suitPair) pairsOfindices
    changedMMatrix = foldl (flip (setElem 0)) mMatrix indicesToChange

    res = prune eComp queryGraph targetGraph changedMMatrix currentRow

-- | Returns True if we can map all neighbors of query vertex to neighbors of target vertex in mMatrix.
--
hasSuitableNeighbors :: forall v1 v2 e1 e2. (Eq e1, Eq e2) => EComparator e1 e2
                                                           -> GenericGraphIso v1 e1
                                                           -> GenericGraphIso v2 e2
                                                           -> Matrix Int
                                                           -> Int
                                                           -> Int
                                                           -> Bool
hasSuitableNeighbors eComp queryGraph targetGraph mMatrix query target = doesSatisfy
  where
    iQ = query - 1
    iT = target - 1

    neighborsOfQ = (\(i, e) -> (iQ, i, e)) <$> queryGraph !. iQ
    neighborsOfT = (\(i, e) -> (iT, i, e)) <$> targetGraph !. iT

    hasProperNeighbor :: GraphEdge e1 -> Bool
    hasProperNeighbor edge = any (\edge' -> getProperElem edge edge' == 1 && eComp edge edge') neighborsOfT

    getProperElem :: GraphEdge e1 -> GraphEdge e2 -> Int
    getProperElem (_, b, _) (_, b', _) = getElem (b + 1) (b' + 1) mMatrix

    doesSatisfy = all hasProperNeighbor neighborsOfQ

-- | Checks whether mMatrix encodes an isomorphism between pMatrix and gMatrix.
--
isIsomorphism :: Matrix Int -- ^ gMatrix
              -> Matrix Int -- ^ pMatrix
              -> Matrix Int -- ^ mMatrix
              -> Bool
isIsomorphism gMatrix pMatrix mMatrix = leqMatrices pMatrix check
  where
    check = multStd mMatrix (transpose (multStd mMatrix gMatrix))

-- | Componentwise "less or equal" operation for matrices.
--
leqMatrices :: Matrix Int -> Matrix Int -> Bool
leqMatrices matrixA matrixB = nrows matrixA * ncols matrixA <= nrows matrixB * ncols matrixB && helper elems
  where
    numOfRows = nrows matrixA
    numOfColumns = ncols matrixB
    elems = [(i, j) | i <- [1..numOfRows], j <- [1..numOfColumns]]
    helper = foldr (\x -> (&&) (uncurry getElem x matrixA <= uncurry getElem x matrixB)) True

-- | Replace all elements in row with 0 apart from chosen one.
--
changeRow :: Matrix Int -> Int -> Int -> Matrix Int
changeRow mMatrix row column = mapRow helper row mMatrix
  where helper column' a = if column' /= column then 0 else a

hasEmptyRow :: Matrix Int -> Bool
hasEmptyRow prunedMatrix = cond
  where
    numberOfRows = nrows prunedMatrix
    cond = any (\x -> all (== 0) (getRow x prunedMatrix)) [1 .. numberOfRows]
