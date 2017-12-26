module Math.Grads.Algo.Cycles
  ( findCycles
  , findLocalCycles
  , getCyclic
  , isEdgeInCycle
  ) where

import           Control.Monad.State         (State, runState)
import           Control.Monad.State.Class   (get, modify)
import           Data.List                   (partition, sort, union, (\\))
import           Data.Set                    (Set)
import qualified Data.Set                    as S (empty, fromList, insert,
                                                   member)
import           Math.Grads.Algo.Interaction (getEnds, getIndices, getOtherEnd,
                                              getVertexIncident, haveSharedEdge)
import           Math.Grads.Algo.Paths       (dfsAllPaths)
import           Math.Grads.Algo.Traversals  (dfsSt)
import           Math.Grads.GenericGraph     (GenericGraph, safeIdx)
import           Math.Grads.Graph            (EdgeList, GraphEdge, vCount)

-- | Takes adjacency list and finds non-redundant set of simple cycles
-- | Cycles sharing in common one edge are considered to be one cycle
-- | BondList must obey rule (b, e, _) b < e
findCycles :: Ord e => [GraphEdge e] -> [[GraphEdge e]]
findCycles bonds = sort <$> conjRings redundantCycles
  where
    redundantCycles = findCyclesR bonds

    findCyclesR :: Ord e => [GraphEdge e] -> [[GraphEdge e]]
    findCyclesR bs = let (result, taken) = stateCycles bs in
      if sort taken == sort bs then result
      else result ++ findCyclesR (bs \\ taken)

    stateCycles :: Ord e => [GraphEdge e] -> ([[GraphEdge e]], [GraphEdge e])
    stateCycles bs = runState (cyclesHelper bs [] (minimum (getIndices bs))) []

conjRings :: Ord e => [[GraphEdge e]] -> [[GraphEdge e]]
conjRings (b : bs) =
  let
    (shd, rest) = partition (haveSharedEdge b) bs
  in
    case shd of
      [] -> b : conjRings rest
      _  -> conjRings $ foldr union b shd : rest
conjRings b = b

takeCycle :: [GraphEdge e] -> GraphEdge e -> [GraphEdge e]
takeCycle [] _ = error "Take cycle on empty list."
takeCycle bl@((aPop, bPop, _) : _) bn@(aNow, bNow, _) = bn : takeWhile cond bl ++ take 1 (dropWhile cond bl)
  where
    theB | bPop == aNow = bNow
         | bPop == bNow = aNow
         | aPop == bNow = aNow
         | otherwise = bNow
    cond :: GraphEdge e -> Bool
    cond (a', b', _) = theB /= a' && theB /= b'

cyclesHelper :: Eq e => [GraphEdge e] -> [GraphEdge e] -> Int -> State [GraphEdge e] [[GraphEdge e]]
cyclesHelper bs trc n = do
  curSt <- get
  let adjBonds = filter (`notElem` curSt) $ getVertexIncident bs n

  let visited = concatMap getEnds curSt
  let curBondClosures = filter (\b -> getOtherEnd b n `elem` visited) adjBonds
  let furtherBonds = filter (`notElem` curBondClosures) adjBonds

  let procBnd bnd = cyclesHelper bs (bnd : trc) (getOtherEnd bnd n)
  restBondClosures <- mapM (\b -> modify (b:) >>= const (procBnd b)) furtherBonds

  return $ (takeCycle trc <$> curBondClosures) ++ concat restBondClosures

isEdgeInCycle :: Ord e => [GraphEdge e] -> Int -> Bool
isEdgeInCycle bs n = any ((bs !! n) `elem`) $ findCycles bs

-- Finds all cycles of minimal length contained in system of cycles
findLocalCycles :: Eq e => EdgeList e -> [EdgeList e]
findLocalCycles bonds = if null cycles then []
                        else helperFilter (tail res) [head res]
  where
    -- TODO: We need to remove this filter.
    cycles = filter (\x -> length x < 21) (findLocalCycles' bonds)
    res = filter (`filterBigCycles` cycles) cycles

findLocalCycles' :: Eq e => EdgeList e -> [EdgeList e]
findLocalCycles' bonds = concatMap (\(a, b, _) -> dfsAllPaths bonds a b) cycleBonds
  where
    stBonds = dfsSt bonds
    cycleBonds = bonds \\ stBonds

filterBigCycles :: Eq e => EdgeList e -> [EdgeList e] -> Bool
filterBigCycles currentCycle cycles = not (foldl (\x y -> x || currentCycle /= y && length currentCycle > length y && length (filter (`elem` currentCycle) y) > 1) False cycles)

helperFilter :: Eq e => [EdgeList e] -> [EdgeList e] -> [EdgeList e]
helperFilter [] ready = ready
helperFilter (x:xs) ready = if exists x ready then helperFilter xs ready else helperFilter xs (x:ready)
  where
    exists a1 = any (\x' -> length a1 == length x' && all (\(a, b, t) -> (a, b, t) `elem` x' || (b, a, t) `elem` x') a1)

-- Checks whether or not given atom belongs to any cycle. This information is used in SMILES construction.
-- If an atom belongs to a cycle and has double or triple bond, depth-first search will branch to it.
isCyclic :: GenericGraph v e -> Int -> Int -> (Bool, Set Int) -> Int -> (Bool, Set Int)
isCyclic graph target previous (result, visited) current | result = (result, visited)
                                                         | (previous /= (-1)) && (current == target) = (True, visited)
                                                         | current `S.member` visited = (result, visited)
                                                         | otherwise = foldl foldFunc (result, updatedVis) next
  where
    next :: [Int]
    next = filter (/= previous) $ graph `safeIdx` current

    updatedVis :: Set Int
    updatedVis = current `S.insert` visited

    foldFunc :: (Bool, Set Int) -> Int -> (Bool, Set Int)
    foldFunc = isCyclic graph target current

-- Returns the set of all atoms which belong to any cycle.
getCyclic :: GenericGraph v e -> Set Int
getCyclic graph = S.fromList . map fst . filter snd $ zip indices cyclic
  where
    indices :: [Int]
    indices = [0 .. vCount graph - 1]

    cyclic :: [Bool]
    cyclic = map (\ix -> fst $ isCyclic graph ix (-1) (False, S.empty) ix) indices
