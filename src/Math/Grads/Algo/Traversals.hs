module Math.Grads.Algo.Traversals
  ( bfsState
  , dfsCycle
  , dfsSt
  , dfs
  , getComps) where

import           Control.Arrow               ((&&&))
import           Control.Monad.State         (State, execState)
import           Control.Monad.State.Class   (get, put)
import qualified Data.Array                  as A
import           Data.List                   (findIndex)
import           Data.Map                    (Map, keys, (!))
import           Data.Maybe                  (fromJust)

import           Math.Grads.Algo.Interaction (edgeListToMap, getIndices,
                                              getOtherEnd, matchEdges, (~=))
import           Math.Grads.GenericGraph     (GenericGraph, gIndex, subgraph)
import           Math.Grads.Graph            (EdgeList, Graph (..))
import           Math.Grads.Utils            (nub)

-- Classic dfs
dfs :: EdgeList e -> Int -> EdgeList e
dfs bonds ind = if ind `elem` keys graphMap then matchEdges bonds bondsInd else []
  where
    graphMap = edgeListToMap bonds
    bondsInd = dfs' graphMap [ind] [] []

dfs' :: Map Int [Int] -> [Int] -> [Int] -> [(Int, Int)] -> [(Int, Int)]
dfs' _ [] _ bonds = bonds
dfs' gr (cur : rest) vis bs | cur `elem` vis = dfs' gr rest vis bs
                            | otherwise = dfs' gr (gr ! cur ++ rest) (cur : vis) visitedBonds
  where
    visitedBonds = concatMap helper (gr ! cur) ++ bs

    helper :: Int -> [(Int, Int)]
    helper sec = [(cur, sec) | notElem (cur, sec) bs && notElem (sec, cur) bs]

getComps :: Ord v => GenericGraph v e -> [GenericGraph v e]
getComps graph = res
  where
    (_, edges) = toList graph
    comps = getComps' edges [0..length (gIndex graph) - 1] [] []
    res = fmap (subgraph graph) comps

getComps' :: EdgeList e -> [Int] -> [Int] -> [[Int]] -> [[Int]]
getComps' _ [] _ res = res
getComps' edges (x : xs) taken res = if x `elem` taken then getComps' edges xs taken res
                                     else getComps' edges xs (taken ++ newComp) (newComp : res)
  where
    newComp = nub (x : getIndices (dfs edges x))

dfsSt :: EdgeList e -> EdgeList e
dfsSt bonds = matchEdges bonds bondsInd
  where
    graph = edgeListToMap bonds
    bondsInd = dfsSt' graph (keys graph) [] []

findRib :: Map Int [Int] -> [Int] -> Int -> (Int, Int)
findRib graph visited current = (current, if not (null found) then head found else -1)
  where
    found = filter (`elem` visited) (graph ! current)

dfsSt' :: Map Int [Int] -> [Int] -> [Int] -> [(Int, Int)] -> [(Int, Int)]
dfsSt' _ [] _ bonds = bonds
dfsSt' graph (current : toVisit) visited bonds | current `elem` visited = dfsSt' graph toVisit visited bonds
                                               | otherwise = dfsSt' graph ((graph ! current) ++ toVisit) (current:visited) visitedBonds
  where
    visitedBonds = bonds ++ if not (null visited) then [found | snd found /= -1] else []
    found = findRib graph visited current

-- Dfs a cycle
dfsCycle :: A.Array Int [Int] -> [Int] -> [Int] -> [Int]
dfsCycle _ [] visited = visited
dfsCycle graph (current:toVisit) visited | current `elem` visited = dfsCycle graph toVisit visited
                                         | otherwise = dfsCycle graph ((graph A.! current) ++ toVisit) (current:visited)

-- List of (level, (edgeIdx, vertexIdx))
type BFSState = [(Int, (Int, Int))]

bfsState :: (Ord v, Eq e, Show v, Show e, Graph g) => g v e -> EdgeList e -> BFSState -> BFSState -> BFSState
bfsState graph bonds ign start = fst $ execState (bfsState' graph bonds) (ign, start)

-- Traverses graph from a given starting point (queue) in Breadth-first search manner
bfsState' :: (Ord v, Eq e, Show v, Show e, Graph g) => g v e -> EdgeList e -> State (BFSState, BFSState) ()
bfsState' gr bonds = do
  (visited, queue) <- get
  let (visitedL, (visitedB, visitedV)) = (fst &&& unzip . snd) $ unzip visited
  case queue of
        ((curLevel, (curBnd, curNum)) : rest) -> do
          let curInc = (fromJust . (\x -> (~= x) `findIndex` bonds)) <$> gr `incidentIdx` curNum
          let nextBonds = nub $ filter ((`notElem` visitedV) . (`getOtherEnd` curNum) . (bonds !!)) curInc
          let nextLevel = ((`getOtherEnd` curNum) . (bonds !!)) <$> nextBonds
          let nextVisited = zip (curLevel : visitedL) $ zip (curBnd : visitedB) (curNum : visitedV)
          put (nextVisited, rest ++ zip (repeat $ curLevel +1) (zip nextBonds nextLevel))
          bfsState' gr bonds
        _ -> return ()
