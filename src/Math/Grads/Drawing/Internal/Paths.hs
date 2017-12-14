{-# LANGUAGE ScopedTypeVariables #-}

module Math.Grads.Drawing.Internal.Paths (findPaths, getCoordsOfPath) where

import           Data.List                                        (delete, find,
                                                                   intersect,
                                                                   maximumBy,
                                                                   union, (\\))
import           Data.Map.Strict                                  (fromList,
                                                                   (!))
import           Data.Maybe                                       (catMaybes,
                                                                   fromJust,
                                                                   isJust)
import           Data.Ord                                         (comparing)
import           Linear.V2                                        (V2 (..))
import           Math.Angem                                       (alignmentFunc)
import           Math.Grads.Algo.Interaction                      (getIndices, getSharedVertex,
                                                                   getVertexIncident,
                                                                   isIncident)
import           Math.Grads.Algo.Paths                            (dfsSearch, findBeginnings)
import           Math.Grads.Algo.Traversals                       (dfs)
import           Math.Grads.Drawing.Internal.Coords               (Link,
                                                                   bondLength)
import           Math.Grads.Drawing.Internal.CyclesPathsAlignment (bondsToAlignTo)
import           Math.Grads.Drawing.Internal.Utils                (Coord,
                                                                   CoordList,
                                                                   findIncidentCoords,
                                                                   tupleToList)
import           Math.Grads.Graph                                 (EdgeList,
                                                                   GraphEdge)
type BondV2 = (V2 Float, V2 Float)

type PathWithLinks e = (CoordList e, [Link e])

data Path e = Path {
  pStart  :: Int,
  pFinish :: Int,
  pBonds  :: EdgeList e
}

-- Calculates coordinates of path
getCoordsOfPath :: forall e. Eq e => EdgeList e -> CoordList e
getCoordsOfPath bonds = fst (greedy pathWithCoords)
  where
    paths = splitPathIntoLongest bonds []
    pathsWithCoords = fmap (\Path { pBonds=bonds' } -> pathToCoords bonds') paths
    pathWithCoords = zip pathsWithCoords (getLinks paths [])

    getLinks :: [Path e] -> [Int] -> [[Link e]]
    getLinks (x : xs) taken =
      let
        links = findLinkingPoints x taken xs
      in links : getLinks xs (taken `union` fmap fst (countNeighbors links))
    getLinks _ _ = error "No links for path."

    greedy :: [PathWithLinks e] -> PathWithLinks e
    greedy [x]    = x
    greedy (x : xs) = greedy (uniteOnLinkingBonds x xs : filter (helper' x) xs)
    greedy _      = error "Greedy function on an empty list."

    helper' :: PathWithLinks e -> PathWithLinks e -> Bool
    helper' x' y' = null $ (snd <$> snd x') `intersect` (fst <$> fst y')

-- Takes path, list of vertices which have been processed and returns links for each path
findLinkingPoints :: forall e. Path e -> [Int] -> [Path e] -> [Link e]
findLinkingPoints Path { pBonds=list } taken = helper
  where
    getInc :: Int -> EdgeList e
    getInc n = filter (`isIncident` n) list

    couldTake :: Int -> Bool
    couldTake n = n `notElem` taken && (not . null $ getInc n)

    helper :: [Path e] -> [Link e]
    helper [] = []
    helper (Path beg end list' : xs) | couldTake beg = wrapResult beg list' : helper xs
                                     | couldTake end = wrapResult end list' : helper xs
                                     | otherwise = helper xs

    wrapResult :: Int -> EdgeList e -> Link e
    wrapResult n l = (fromJust $ getSharedVertex b1 b2, b2)
      where
        b1 = head $ getInc n
        b2 = head $ getVertexIncident l n

splitPathIntoLongest :: Eq e => EdgeList e -> [Int] -> [Path e]
splitPathIntoLongest [] _ = []
splitPathIntoLongest bonds taken = firstPath : splitPathIntoLongest restBonds newTaken
  where
    ends' = findBeginnings bonds
    ends = filter (`notElem` taken) ends'

    startEnds = if not (null taken) then taken else ends'

    allPaths = filter (not . null) ((\(x, y) -> maybe [] fst (dfsSearch bonds x y)) <$> allPairs startEnds ends)
    allPathsTrue = concatMap (\x -> maybe [x] (splitOnPoint x) (findPointsToSplit x taken)) allPaths

    longestPath = maximumBy (comparing length) allPathsTrue
    [start, finish] = findBeginnings longestPath

    restBonds = bonds \\ longestPath
    firstPath = Path { pStart=start, pFinish=finish, pBonds=longestPath }
    newTaken = taken `union` getIndices longestPath

pathToCoords :: forall e. EdgeList e -> CoordList e
pathToCoords bonds = matchBondsOfPath coords bonds
  where
    angle = pi / 6.0
    radius = bondLength
    allPoints = getPoint <$> concat (repeat [1.0, -1.0])
    dfsRes = dfs bonds (head (findBeginnings bonds))
    coords = zip (getIndicesEdges dfsRes) (buildPath allPoints)

    getPoint :: Float -> V2 Float
    getPoint m = V2 (radius * cos (m * angle)) (radius * sin (m * angle))

    getIndicesEdges :: EdgeList e ->  [Int]
    getIndicesEdges [] = error "Get indices edges on empy list."
    getIndicesEdges [(a, b, _)] = [a, b]
    getIndicesEdges (bnd@(a, b, _) : bnd'@(a', b', _) : xs) = if a == a' || a == b' then b : a : helper (bnd' : xs) bnd
                                                              else a : b : helper (bnd' : xs) bnd

    helper :: EdgeList e -> GraphEdge e -> [Int]
    helper [] _ = error "Get indices edges helper on empty list."
    helper [(a', b', _)] (a, b, _) = if a' == a || a' == b then [b']
                                     else [a']
    helper (bnd@(a, b, _) : bnd'@(a', b', _) : xs) _ = if a == a' || a == b' then a : helper (bnd' : xs) bnd
                                                       else b : helper (bnd' : xs) bnd

buildPath :: [V2 Float] -> [V2 Float]
buildPath [] = []
buildPath (y : ys) = V2 0.0 0.0 : y : helper ys y
  where
    helper :: [V2 Float] -> V2 Float -> [V2 Float]
    helper [] _ = []
    helper (b:xs) b' = let newCoords = b + b' in newCoords : helper xs newCoords

matchBondsOfPath :: forall e. [(Int, V2 Float)] -> EdgeList e -> CoordList e
matchBondsOfPath matches = helper
  where
    mapOfCoords = fromList matches

    helper :: EdgeList e -> CoordList e
    helper [] = []
    helper (bond@(a, b, _) : xs) = (bond, (mapOfCoords ! a, mapOfCoords ! b)) : helper xs

uniteOnLinkingBonds :: forall e. PathWithLinks e -> [PathWithLinks e] -> PathWithLinks e
uniteOnLinkingBonds (mainPath, uniteThis) otherPaths = pathUniter coordsToAdd (mainPath, [])
  where
    counted = countNeighbors uniteThis
    neighbors = fmap getCoordsOfLinks counted

    coordsToAdd = concatMap align neighbors

    align :: (Int, [BondV2]) -> [PathWithLinks e]
    align (ind, toAlignBonds) = alignOnBonds <$> zip toAlignBonds (findNeighbors otherPaths ind)

    getCoordsOfLinks :: (Int, Int) -> (Int, [BondV2])
    getCoordsOfLinks (ind, counts) =
      let
        (leftBond, rightBond) = findAdjacent mainPath ind
      in (ind, bondsToAlignTo leftBond rightBond counts)

    pathUniter :: [PathWithLinks e] -> PathWithLinks e -> PathWithLinks e
    pathUniter [] res                   = res
    pathUniter ((a, b) : xs) (resA, resB) = pathUniter xs (resA ++ a, resB ++ b)

countNeighbors :: forall e. [Link e] -> [(Int, Int)]
countNeighbors list = zip allInds (fmap (numberOfNeighbors list 0) allInds)
  where
    allInds = linkingIndices list []

    linkingIndices :: [Link e] -> [Int] -> [Int]
    linkingIndices [] res = res
    linkingIndices (x : xs) res = if fst x `elem` res then linkingIndices xs res
                                  else linkingIndices xs (fst x : res)

    numberOfNeighbors :: [Link e] -> Int -> Int -> Int
    numberOfNeighbors [] acc _ = acc
    numberOfNeighbors (x : xs) acc ind = if fst x == ind then numberOfNeighbors xs (acc + 1) ind
                                         else numberOfNeighbors xs acc ind

splitOnMultiplePoints :: forall e. Eq e => EdgeList e -> [Int] -> [EdgeList e]
splitOnMultiplePoints bonds cut = helper [bonds] cut []
  where
    helper :: [EdgeList e] -> [Int] -> [EdgeList e] -> [EdgeList e]
    helper [] _ _ = error "Split on multiple points helper on empty list."
    helper lastCut [] res = res ++ lastCut
    helper (x : xs) (y : ys) res = if y `elem` getIndices x then helper (splitOnPoint x y ++ xs) ys res
                                   else helper xs (y : ys) (x : res)

splitOnPoint :: forall e. Eq e => EdgeList e -> Int -> [EdgeList e]
splitOnPoint list point = filter (not . null) foundNeighbors
  where
    foundNeighbors = fmap splitter list

    splitter :: GraphEdge e -> EdgeList e
    splitter bond@(a, b, _) | a == point = bond : dfs (delete bond list) b
                            | b == point = bond : dfs (delete bond list) a
                            | otherwise = []

allPairs :: [Int] -> [Int] -> [(Int, Int)]
allPairs starts ends = concatMap (\start -> fmap (\end -> (start, end)) ends) starts

-- This function is used for splitting one path into substantial pieces during calculation of coordinates of this path
findPointsToSplit :: forall e. EdgeList e -> [Int] -> Maybe Int
findPointsToSplit _ [] = Nothing
findPointsToSplit [] _ = Nothing
findPointsToSplit [_] _ = Nothing
findPointsToSplit list taken = helper ((tail . init) list)
  where
    helper :: EdgeList e -> Maybe Int
    helper [] = Nothing
    helper ((a, b, _) : xs) | a `elem` taken = Just a
                            | b `elem` taken = Just b
                            | otherwise = helper xs

-- This function is used for splitting one path into several paths if one vertex of path belongs to cycle
findPointsToSplitHard :: forall e. Eq e => EdgeList e -> [Int] -> [Int]
findPointsToSplitHard _ [] = []
findPointsToSplitHard [] _ = []
findPointsToSplitHard [_] _ = []
findPointsToSplitHard list taken = helper list []
  where
    helper :: EdgeList e -> [Int] -> [Int]
    helper [] acc = acc
    helper (x@(a, b, _) : xs) acc  | a `notElem` acc && a `elem` taken && hasNeighbor a x = helper xs (a : acc)
                                   | b `notElem` acc && b `elem` taken && hasNeighbor b x = helper xs (b : acc)
                                   | otherwise = helper xs acc

    hasNeighbor :: Int -> GraphEdge e ->  Bool
    hasNeighbor ind x = isJust (find (\bond -> bond /= x && isIncident bond ind) list)

-- Finds all paths between cycles in graph
findPaths :: Eq e => EdgeList e -> [Int] -> [EdgeList e]
findPaths [] _ = []
findPaths bonds [] = [bonds]
findPaths bonds taken = allPathsTrue
  where
    paths = findPaths' bonds
    allPathsTrue = concatMap (\x -> let cut = findPointsToSplitHard x taken in splitOnMultiplePoints x cut) paths

findPaths' :: Eq e => EdgeList e -> [EdgeList e]
findPaths' [] = []
findPaths' bonds@(x : _) = newPath : findPaths' (filter (`notElem` newPath) bonds)
  where
    newPath = findPath bonds bonds [x] x

findPath :: Eq e => EdgeList e -> EdgeList e -> EdgeList e -> GraphEdge e -> EdgeList e
findPath [] _ found _ = found
findPath (x@(a, b, _) : xs) bonds found pathFrom@(src, dst, _) = if cond then findPath xs bonds newFound pathFrom
                                                                 else findPath xs bonds found pathFrom
  where
    cond = (a == dst || b == src || a == src || b == dst) && (x `notElem` found)
    newFound = findPath bonds bonds (found ++ [x]) x

alignOnBonds :: (BondV2, (BondV2, PathWithLinks e)) -> PathWithLinks e
alignOnBonds (coordsA, (coordsB, (list, toSave))) = (resCoords, toSave)
  where
    align = alignmentFunc (tupleToList coordsA) (tupleToList coordsB)
    resCoords = fmap (\(bond, (coordA', coordB')) -> (bond, (align coordA', align coordB'))) list

findNeighbors :: [PathWithLinks e] -> Int -> [(BondV2, PathWithLinks e)]
findNeighbors [] _ = []
findNeighbors (x : xs) ind = if not (null found) then (head found, x) : findNeighbors xs ind
                             else findNeighbors xs ind
  where
    found = catMaybes (fmap coordsOfAdjacentBond (fst x))

    coordsOfAdjacentBond :: Coord e -> Maybe BondV2
    coordsOfAdjacentBond ((a, b, _), (coordsA, coordsB)) | a == ind = Just (coordsA, coordsB)
                                                         | b == ind = Just (coordsB, coordsA)
                                                         | otherwise = Nothing

findAdjacent :: CoordList e -> Int -> (Coord e, Coord e)
findAdjacent list ind = (leftNeighbor, rightNeighbor)
  where
    [leftNeighbor, rightNeighbor] = take 2 (findIncidentCoords ind list)
