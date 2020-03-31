{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.Grads.Algo.SSSR
  ( findSSSR
  ) where

import           Control.Arrow              ((***))
import           Control.Lens               (over, to, toListOf, (%~), (&), _1, _2)
import           Data.Bimap                 (Bimap, (!>))
import           Data.List                  (intersect, nub, sort)
import           Data.List.Index            (ifoldl)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M (empty, insert, member, (!))
import           Data.Matrix                (Matrix, matrix, unsafeGet,
                                             unsafeSet)
import qualified Data.Set                   as S

import           Math.Grads.Algo.Cycles     (getCyclic)
import           Math.Grads.Algo.Traversals (getCompsWithReindex)
import           Math.Grads.GenericGraph    (GenericGraph, subgraphWithReindex)
import           Math.Grads.Graph           (EdgeList, toList)


-- | RP-Path algorithm for searching the smallest set of smallest rings.
-- <https://www.ncbi.nlm.nih.gov/pubmed/19805142>

-- | Find SSSR of any graph.
--
findSSSR :: (Ord v, Ord e) => GenericGraph v e -> [EdgeList e]
findSSSR graph = sssr
  where
    (reindex, cyclicGraph) = subgraphWithReindex graph . S.toList $ getCyclic graph
    sssr = reindexCycles reindex $ findCyclicSSSR cyclicGraph

-- | Find SSSR of cyclic graph, i.e. each graphs edge belongs at least 1 cycle.
--
findCyclicSSSR :: forall v e. (Ord v, Ord e) => GenericGraph v e -> [EdgeList e]
findCyclicSSSR cyclicGraph = sssr
  where
    reindex2compList :: [(Bimap Int Int, GenericGraph v e)]
    reindex2compList = getCompsWithReindex cyclicGraph

    sssr = reindex2compList & traverse %~ over _2 findFusedSSSR
                            & traverse %~ uncurry reindexCycles
                            & concat

-- | Find SSSR of fused graph, i.e. graph is cyclic and connected.
--
findFusedSSSR :: (Ord v, Ord e) => GenericGraph v e -> [EdgeList e]
findFusedSSSR fusedGraph = sssr
  where
    g@(_, edges) = toList fusedGraph
    (n, m)       = (length *** length) g
    maxSSSRs     = m - n + 1

    edgeIndex :: Map (Int, Int) Int
    edgeIndex = ifoldl insertEdge M.empty edges
      where
        insertEdge edgeMap ind (x, y, _) = M.insert (y, x) ind $ M.insert (x, y) ind edgeMap

    (pid, pid') = calculatePidMatrices n m edgeIndex
    sssrEdges   = takeSSSR n maxSSSRs pid pid'
    sssr        = fmap (edges !!) <$> sssrEdges

takeSSSR :: Int -> Int -> Matrix (Int, [[Int]]) -> Matrix [[Int]] -> [[Int]]
takeSSSR n maxSSSRs pid pid' = go [] [] 3 (1, 1)
  where
    go cycles edges len (i, j)
      | (i, j) > (n, n) || length cycles >= maxSSSRs =
          cycles
      | curLen /= len || length ij < 1 || length ij == 1 && null ij' =
          go cycles edges nextLen nextInd
      | otherwise =
          go nextCycles nextEdges nextLen nextInd
      where
        (ijDist, ij) = unsafeGet i j pid
        ij'          = unsafeGet i j pid'

        curLen  = ijDist * 2 + len `mod` 2

        newCycles = take (maxSSSRs - length cycles) $
          filter (`notElem` cycles) $
          filter (\c -> all (c `notContains`) cycles) $
          nub $
          cartesianProduct ij (if even len then ij else ij') notIntersects concatSort
          where
            notIntersects a b = null $ intersect a b
            concatSort    a b = sort $ a ++ b
            notContains   a b = length (a `intersect` b) < length b - 1

        nextCycles = cycles ++ newCycles
        nextEdges  = nub $ edges ++ concat newCycles

        (nextLen, nextInd)
          | (i, j) == (n, n) = (len + 1, (1,     1))
          | j == n           = (len,     (i + 1, 1))
          | otherwise        = (len,     (i,     j + 1))

calculatePidMatrices :: Int -> Int -> Map (Int, Int) Int
                     -> (Matrix (Int, [[Int]]), Matrix [[Int]])
calculatePidMatrices n m edgeIndex = calcPids initPid initPid' (1, 1, 1)
  where
    initPid :: Matrix(Int, [[Int]])
    initPid = matrix n n $ \(x, y) -> if
      | (x - 1, y - 1) `M.member` edgeIndex -> (1, [[edgeIndex M.! (x - 1, y - 1)]])
      | otherwise                           -> (m + 1, [])

    initPid' :: Matrix [[Int]]
    initPid' = matrix n n $ const []

    calcPids :: Matrix(Int, [[Int]]) -> Matrix [[Int]]
             -> (Int, Int, Int)
             -> (Matrix(Int, [[Int]]), Matrix [[Int]])
    calcPids pid pid' ind@(k, i, j)
      | ind > (n, n, n) = (pid, pid')
      | otherwise       = calcPids nextPid nextPid' nextInd
      where
        (ijDist, ij) = unsafeGet i j pid
        (ikDist, ik) = unsafeGet i k pid
        (kjDist, kj) = unsafeGet k j pid
        ikjDist      = ikDist + kjDist
        ikj          = cartesianProduct ik kj (/=) (++)
        ij'          = unsafeGet i j pid'

        nextPid
          | ijDist >  ikjDist = unsafeSet (ikjDist, ikj)       (i, j) pid
          | ijDist == ikjDist = unsafeSet (ijDist,  ij ++ ikj) (i, j) pid
          | otherwise         = pid

        nextPid'
          | ijDist == ikjDist + 1 = unsafeSet ij           (i, j) pid'
          | ijDist == ikjDist - 1 = unsafeSet (ij' ++ ikj) (i, j) pid'
          | ijDist >  ikjDist + 1 = unsafeSet []           (i, j) pid'
          | otherwise             = pid'

        nextInd
          | (i, j) == (n, n) = (k + 1, 1,     1    )
          | j == n           = (k,     i + 1, 1    )
          | otherwise        = (k,         i, j + 1)

reindexCycles :: Bimap Int Int -> [EdgeList e] -> [EdgeList e]
reindexCycles reindex = fmap reindexCycle
  where
    reindexCycle :: EdgeList e -> EdgeList e
    reindexCycle = toListOf $ traverse . to (over _1 (reindex !>) . over _2 (reindex !>))

cartesianProduct :: Eq a => [a] -> [a] -> (a -> a -> Bool) -> (a -> a -> b) -> [b]
cartesianProduct xs ys f g = [ g x y | x <- xs, y <- ys, f x y ]
