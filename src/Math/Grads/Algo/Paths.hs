module Math.Grads.Algo.Paths
  ( allPathsInGraph
  , allPathsFromVertex
  , dfsAllPaths
  , dfsSearch
  , findBeginnings
  ) where

import           Control.Monad               (forM_)
import           Control.Monad.State         (State, execState)
import           Control.Monad.State.Class   (get, modify)
import           Data.Map                    (Map, keys, (!))
import           Data.Maybe                  (isJust)

import           Math.Grads.Algo.Interaction (edgeListToMap, getVertexAdjacent,
                                              matchEdges, sortBondList)
import           Math.Grads.GenericGraph     (GenericGraph)
import           Math.Grads.Graph            (EdgeList, Graph (..))
import           Math.Grads.Utils            (nub, subsets, uniter)

-- Finds all vertices that have only one neighbor
findBeginnings :: EdgeList e -> [Int]
findBeginnings bonds = fmap fst (filter (\x -> snd x == 1) counters)
  where
    graph = edgeListToMap bonds
    counters = zip (keys graph) (map (\x -> length (graph ! x)) (keys graph))

-- Calculates all branched paths in graph up to given length
allPathsInGraph :: Ord e => GenericGraph v e -> Int -> [EdgeList e]
allPathsInGraph molGraph lengthOfPath = helper molGraph atomInds []
  where
    atomInds = [0 .. (vCount molGraph - 1)]

    helper :: Ord e => GenericGraph v e -> [Int] -> [Int] -> [EdgeList e]
    helper _ [] _ = []
    helper gr (x : xs) forbidden = allPathsFromVertex gr x lengthOfPath forbidden ++ helper gr xs (x : forbidden)

allPathsFromVertex :: Ord e => GenericGraph v e -> Int -> Int -> [Int] -> [EdgeList e]
allPathsFromVertex molGraph atom lengthOfPath forbidden = nub filtered
  where
   res' = execState (allPathsFromVertexSt molGraph [atom] lengthOfPath forbidden []) []
   filtered = sortBondList <$> filter (not . null) res'

allPathsFromVertexSt :: Ord e => GenericGraph v e -> [Int] -> Int -> [Int] -> EdgeList e -> State [EdgeList e] [EdgeList e]
allPathsFromVertexSt molGraph atoms lenOfPath forbidden res = if lenOfPath < 0 then get
                                                              else
  do
    modify (res :)

    let bondsNeigh = nub (filter (`notElem` res) (concatMap (incidentIdx molGraph) atoms))
    let allowedBondsNeigh = filter (\(a, b, _) -> a `notElem` forbidden && b `notElem` forbidden) bondsNeigh
    let bondSets = filter ((\x -> x > 0 && x <= lenOfPath) . length) (subsets allowedBondsNeigh)
    if lenOfPath == 0 || not (null allowedBondsNeigh) then
      do
        forM_ bondSets (\set -> do
          let newNeighbors = concatMap (getVertexAdjacent set) atoms
          let newLength = lenOfPath - length set
          let newRes = res ++ set
          modify (execState (allPathsFromVertexSt molGraph newNeighbors newLength forbidden newRes) [] ++))
        get
    else get

-- Finds path between two vertices in graph represented as list of bonds. Graph shouldn't have any cycles
dfsSearch :: EdgeList e -> Int -> Int -> Maybe (EdgeList e, [Int])
dfsSearch bonds start finish = if start `elem` inds && finish `elem` inds then Just (if not (null x) then matchEdges bonds bondsInd else [], x)
                               else Nothing
  where
    graph = edgeListToMap bonds
    Just x = let res = helperDfs graph (-1) finish [start] in if isJust res then res else Just []
    bondsInd = uniter x
    inds = concatMap (\(x', y, _) -> [x', y]) bonds

helperDfs :: Map Int [Int] -> Int -> Int -> [Int] -> Maybe [Int]
helperDfs graph prev finish path | current /= prev && current /= finish = if not (null (==?)) then head (==?) else Nothing
                                 | current == finish = Just path
                                 | otherwise = Nothing
  where
    current = head path
    children = filter (/= prev) (graph ! current)
    (==?) = filter isJust (map (\x -> helperDfs graph current finish (x : path)) children)

dfsAllPaths :: EdgeList e -> Int -> Int -> [EdgeList e]
dfsAllPaths bonds start finish = map (matchEdges bonds) bondsInd
  where
    graph = edgeListToMap bonds
    paths = execState (statePaths graph finish [start]) []
    filteredPaths = filter (\x -> length x > 2) paths
    bondsInd = map (\x -> let united = uniter x in if (start, finish) `elem` united then united else (start, finish) : united) filteredPaths

statePaths :: Map Int [Int] -> Int -> [Int] -> State [[Int]] [[Int]]
statePaths graph finish path = if head path `elem` tail path then get else (do
  let current = head path
  if current == finish then do {modify ([path] ++); get} else
     do
      let children = filter (`notElem` path) (graph ! current)
      forM_ children (\child -> modify (execState (statePaths graph finish (child:path)) [] ++))
      get)
