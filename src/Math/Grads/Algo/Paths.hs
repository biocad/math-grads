-- | Module that provides functions for different kinds of path-finding in graph.
--
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
import           Data.Maybe                  (fromMaybe, isJust)
import           Math.Grads.Algo.Interaction (edgeListToMap, getVertexAdjacent,
                                              matchEdges, sortBondList)
import           Math.Grads.GenericGraph     (GenericGraph)
import           Math.Grads.Graph            (EdgeList, Graph (..))
import           Math.Grads.Utils            (nub, subsets, uniter)

-- | Finds all vertices in 'EdgeList' that have only one neighbour.
--
findBeginnings :: EdgeList e -> [Int]
findBeginnings edges = fmap fst (filter ((== 1) . snd) counters)
  where
    graph    = edgeListToMap edges
    counters = zip (keys graph) (fmap (length . (graph !)) (keys graph))

-- | Calculates all branched paths in graph up to the given length.
--
allPathsInGraph :: Ord e => GenericGraph v e -> Int -> [EdgeList e]
allPathsInGraph graph lengthOfPath = helper graph vertexInds []
  where
    vertexInds = [0 .. (vCount graph - 1)]

    helper :: Ord e => GenericGraph v e -> [Int] -> [Int] -> [EdgeList e]
    helper _ [] _ = []
    helper gr (x : xs) forbidden = allPathsFromVertex gr x lengthOfPath forbidden ++ helper gr xs (x : forbidden)

-- | Calculates all branched paths up to the given length from given vertex in graph
-- considering indices of vertices that shouldn't be visited during path-finding.
--
allPathsFromVertex :: Ord e => GenericGraph v e -> Int -> Int -> [Int] -> [EdgeList e]
allPathsFromVertex graph vertex lengthOfPath forbidden = nub filtered
  where
   res' = execState (allPathsFromVertexSt graph [vertex] lengthOfPath forbidden []) []
   filtered = sortBondList <$> filter (not . null) res'

allPathsFromVertexSt :: Ord e => GenericGraph v e -> [Int] -> Int -> [Int] -> EdgeList e -> State [EdgeList e] [EdgeList e]
allPathsFromVertexSt graph vertices lenOfPath forbidden res = if lenOfPath < 0 then get
                                                              else
  do
    modify (res :)

    let edgesNeigh = nub (filter (`notElem` res) (concatMap (incidentIdx graph) vertices))
    let allowedEdgesNeigh = filter (\(a, b, _) -> a `notElem` forbidden && b `notElem` forbidden) edgesNeigh
    let edgeSets = filter ((\x -> x > 0 && x <= lenOfPath) . length) (subsets allowedEdgesNeigh)
    if lenOfPath == 0 || not (null allowedEdgesNeigh) then
      do
        forM_ edgeSets (\set -> do
          let newNeighbors = concatMap (getVertexAdjacent set) vertices
          let newLength = lenOfPath - length set
          let newRes = res ++ set
          modify (execState (allPathsFromVertexSt graph newNeighbors newLength forbidden newRes) [] ++))
        get
    else get

-- | Finds path between two vertices in graph represented as 'EdgeList'.
-- Graph shouldn't have any cycles. Hmmm, what's the difference between this function
-- and DFS or BFS?..
--
dfsSearch :: EdgeList e -> Int -> Int -> Maybe (EdgeList e, [Int])
dfsSearch edges start finish = if cond then Just (matchEdges edges edgesInd, x)
                               else Nothing
  where
    graph = edgeListToMap edges
    x     = fromMaybe [] $ helperDfs graph (-1) finish [start]

    edgesInd = uniter x
    inds     = concatMap (\(x', y, _) -> [x', y]) edges

    cond = start `elem` inds && finish `elem` inds

helperDfs :: Map Int [Int] -> Int -> Int -> [Int] -> Maybe [Int]
helperDfs graph prev finish path | current /= prev && current /= finish = if not (null (==?)) then head (==?) else Nothing
                                 | current == finish = Just path
                                 | otherwise = Nothing
  where
    current = head path
    children = filter (/= prev) (graph ! current)
    (==?) = filter isJust (map (\x -> helperDfs graph current finish (x : path)) children)

-- | Finds all paths between vertices with given indices in 'EdgeList'.
--
dfsAllPaths :: EdgeList e -> Int -> Int -> [EdgeList e]
dfsAllPaths edges start finish = fmap (matchEdges edges) edgesInd
  where
    graph = edgeListToMap edges
    paths = execState (statePaths graph finish [start]) []

    filteredPaths = filter ((> 2) . length) paths
    edgesInd = fmap helper filteredPaths

    helper :: [Int] -> [(Int, Int)]
    helper l = if (start, finish) `elem` united then united
               else (start, finish) : united
      where
        united = uniter l

statePaths :: Map Int [Int] -> Int -> [Int] -> State [[Int]] [[Int]]
statePaths graph finish path = if head path `elem` tail path then get else (do
  let current = head path
  if current == finish then do {modify ([path] ++); get} else
     do
      let children = filter (`notElem` path) (graph ! current)
      forM_ children (\child -> modify (execState (statePaths graph finish (child : path)) [] ++))
      get)
