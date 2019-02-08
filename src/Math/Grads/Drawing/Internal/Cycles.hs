{-# LANGUAGE ScopedTypeVariables #-}

-- | Module that calculates coordinates of systems of conjugated cycles in graph.
--
module Math.Grads.Drawing.Internal.Cycles
  ( getCoordsOfGlobalCycle
  ) where

import qualified Data.Array                                       as A
import           Data.List                                        (find,
                                                                   groupBy, nub,
                                                                   sortOn)
import qualified Data.Map.Strict                                  as M
import           Data.Maybe                                       (catMaybes,
                                                                   fromJust,
                                                                   isJust,
                                                                   mapMaybe)
import           Linear.Metric                                    (distance,
                                                                   norm)
import           Linear.V2                                        (V2 (..))
import           Linear.Vector                                    ((*^), (^/))
import           Math.Grads.Algo.Cycles                           (findLocalCycles)
import           Math.Grads.Algo.Interaction                      (getEnds,
                                                                   getIndices)
import           Math.Grads.Algo.Paths                            (findBeginnings)
import           Math.Grads.Algo.Traversals                       (dfsCycle)
import           Math.Grads.Angem                                 (alignmentFunc)
import           Math.Grads.Drawing.Internal.Coords               (Link,
                                                                   bondLength)
import           Math.Grads.Drawing.Internal.CyclesPathsAlignment (bondsToAlignTo,
                                                                   bondsToAlignToExtreme)
import           Math.Grads.Drawing.Internal.Utils                (Coord,
                                                                   CoordList,
                                                                   centroid,
                                                                   cleanCoordList,
                                                                   cleanListOfCoordLists,
                                                                   compareCoords,
                                                                   findIncidentCoords,
                                                                   reflectCycle,
                                                                   tupleToList)
import           Math.Grads.GenericGraph                          (gAdjacency,
                                                                   gIndex)
import           Math.Grads.Graph                                 (EdgeList,
                                                                   GraphEdge,
                                                                   fromList,
                                                                   vCount)
import           Math.Grads.Utils                                 (uniter)

-- | Calculates coordinates of system of cycles and coordinates of edges that are adjacent to it.
--
getCoordsOfGlobalCycle :: Eq e => [CoordList e] -> EdgeList e -> Maybe (CoordList e)
getCoordsOfGlobalCycle paths globalCycle = if not (null localCycles) && isJust alignedM then Just res
                                           else Nothing
  where
    localCycles = findLocalCycles globalCycle

    localCyclesWithCoords = sortOn (\x -> - (length x)) (fmap getCoordsOfLocalCycle localCycles)
    alignedM = greedyAlignmentOfLocalCycles [head localCyclesWithCoords] (tail localCyclesWithCoords)

    aligned = fromJust alignedM
    cleanAligned = cleanCoordList (concat aligned) []

    res = restoreEndsForCycle cleanAligned paths aligned

getCoordsOfLocalCycle :: EdgeList e -> CoordList e
getCoordsOfLocalCycle thisCycle = matchBonds thisCycle (getCoordsOfPolygon (length thisCycle))

getCoordsOfPolygon :: Int -> [(V2 Float, V2 Float)]
getCoordsOfPolygon number = let coords = fmap getPoint [0..number - 1] in (last coords, head coords) : uniter coords
  where
    angle = 2 * pi / fromIntegral number
    radius = bondLength / sin (angle / 2) / 2

    getPoint :: Int -> V2 Float
    getPoint step = V2 (radius * cos (fromIntegral step * angle)) (radius * sin (fromIntegral step * angle))

uniteLocalCyclesOnBond :: Coord e -> Coord e -> CoordList e -> CoordList e
uniteLocalCyclesOnBond (_, coords) (_, coords') toTransformCoords = transformFuncCoord <$> toTransformCoords
  where
    transformFunc' = alignmentFunc (tupleToList coords) (tupleToList coords')
    transformFuncCoord (bond, (a, b)) = (bond, (transformFunc' a, transformFunc' b))

matchBonds :: EdgeList e -> [(V2 Float, V2 Float)] -> CoordList e
matchBonds bonds coords = matchBonds' bonds (zip bondsInd coords)
  where
    vertices = nub $ concatMap getEnds bonds
    index = M.fromList (zip vertices [0..])
    graph = fromList (vertices, fmap (\(a, b, t) -> (index M.! a, index M.! b, t)) bonds)
    graphArray = fmap fst <$> gAdjacency graph

    inds = (gIndex graph A.!) <$> dfsCycle graphArray [0 .. (vCount graph - 1)] []
    bondsInd = uniter inds ++ [(last inds, head inds)]

matchBonds' :: EdgeList e -> [((Int, Int), (V2 Float, V2 Float))] -> CoordList e
matchBonds' bonds match = fmap (changeCoords match) bonds

changeCoords :: [((Int, Int), (V2 Float, V2 Float))] -> GraphEdge e -> Coord e
changeCoords [] _ = error "No matching coords in changeCoords function."
changeCoords (((a', b'), (left, right)) : xs) bond@(a, b, _) | a == a' && b == b' = (bond, (left, right))
                                                             | a == b' && b == a' = (bond, (right, left))
                                                             | otherwise = changeCoords xs bond

greedyAlignmentOfLocalCycles :: forall e. Eq e => [CoordList e] -> [CoordList e] -> Maybe [CoordList e]
greedyAlignmentOfLocalCycles mainCycles [] = Just mainCycles
greedyAlignmentOfLocalCycles mainCycles xs = if isJust idOfNeighborM then res
                                             else Nothing
  where
    neighborExists = fmap checkForNeighbor xs
    idOfNeighborM = helper neighborExists 0

    idOfNeighbor = fromJust idOfNeighborM
    neighbor = (xs !! idOfNeighbor)

    x = concat mainCycles
    matches = catMaybes (concatMap findMatchingBond x)
    (coordsA, coordsB) = head matches

    reflectedIfNeeded = reflectIfIntersects (uniteLocalCyclesOnBond coordsA coordsB neighbor) mainCycles (snd coordsA)
    finalCycle = correctLeftMatches (snd <$> tail matches) reflectedIfNeeded x

    res = greedyAlignmentOfLocalCycles (finalCycle : mainCycles) (take idOfNeighbor xs ++ drop (idOfNeighbor + 1) xs)

    findMatchingBond :: Coord e -> [Maybe (Coord e, Coord e)]
    findMatchingBond thisBond  = fmap (hasMatch thisBond) neighbor

    hasMatch :: Coord e -> Coord e -> Maybe (Coord e, Coord e)
    hasMatch thisBond otherBond = if compareCoords thisBond otherBond then Just (thisBond, otherBond)
                                  else Nothing

    checkForNeighbor :: CoordList e -> Bool
    checkForNeighbor = any (\otherCoord -> any (compareCoords otherCoord) x)

    helper :: [Bool] -> Int -> Maybe Int
    helper [] _             = Nothing -- No neighbors for cycle in one conjugated cycle with it.
                                      -- Theoretically it is impossible, but due to the nature of our findLocalCycles function this can happen
    helper (y : ys) counter = if y then Just counter else helper ys (counter + 1)

reflectIfIntersects :: CoordList e -> [CoordList e] -> (V2 Float, V2 Float) -> CoordList e
reflectIfIntersects thisCycle allCycles (coordA, coordB) = if intersects then reflectCycle thisCycle (coordA, coordB)
                                                           else thisCycle
  where
    thisCentroid = centroid thisCycle
    otherCentroids = centroid <$> allCycles
    intersects = any (\x -> distance x thisCentroid <= bondLength) otherCentroids

correctLeftMatches :: forall e. Eq e => [Coord e] -> CoordList e -> CoordList e -> CoordList e
correctLeftMatches [] thisCycle _ = thisCycle
correctLeftMatches ((bond@(beg, end, _), _) : xs) thisCycle mainCycles = correctLeftMatches xs thisCycleUpdated mainCycles
  where
    thisCycleUpdated = catMaybes (fmap correctMatch thisCycle)

    correctMatch :: Coord e -> Maybe (Coord e)
    correctMatch coord@(bond'@(a, b, t'), (coordA, coordB)) | bond == bond' = Nothing
                                                            | beg == a || end == a = Just ((a, b, t'), (substitute coordA a, coordB))
                                                            | beg == b || end == b = Just ((a, b, t'), (coordA, substitute coordB b))
                                                            | otherwise = Just coord

    substitute :: V2 Float -> Int -> V2 Float
    substitute varCoord endToFix =
      let
        x = mapMaybe (helper endToFix) mainCycles
      in if not (null x) then head x
         else varCoord

    helper :: Int -> Coord e -> Maybe (V2 Float)
    helper endToFix ((a', b', _), (coordA', coordB')) | a' == endToFix = Just coordA'
                                                      | b' == endToFix = Just coordB'
                                                      | otherwise = Nothing

restoreEndsForCycle :: Eq e => CoordList e -> [CoordList e] -> [CoordList e] -> CoordList e
restoreEndsForCycle thisCycle [[]] _ = thisCycle
restoreEndsForCycle thisCycle paths localCycles = thisCycle ++ concat neighbors
  where
    verticesOfCycle = getIndices (fmap fst thisCycle)
    cycleLinkingCoords = mapMaybe (findLinks verticesOfCycle) paths
    counted = countNeighbors' cycleLinkingCoords
    neighbors = fmap (getLinksWithCoords thisCycle localCycles) counted

countNeighbors' :: [(Int, GraphEdge e)] -> [(Int, EdgeList e)]
countNeighbors' list = (\x -> let (a, b) = unzip x in (head a, b)) <$> groupBy (\a b -> fst a == fst b) list

findLinks :: [Int] -> CoordList e -> Maybe (Link e)
findLinks verticesOfCycle path = if not (null found) then Just (foundVertex, fst (fromJust bond))
                                 else Nothing
  where
    found = filter (`elem` verticesOfCycle) (findBeginnings (fmap fst path))
    foundVertex = head found
    bond = find (\((a, b, _), _) -> a == foundVertex || b == foundVertex) path

getLinksWithCoords :: forall e. Eq e => CoordList e -> [CoordList e] -> (Int, EdgeList e) -> CoordList e
getLinksWithCoords thisCycle localCycles (ind, bonds) = res
  where
    found = findAdjacentBondsCycles thisCycle localCycles ind

    bondsLength  = length bonds
    alignedBonds = either (\(f, s) -> bondsToAlignTo f s bondsLength) (flip bondsToAlignToExtreme bondsLength) found

    res = assignCoords bonds alignedBonds ind

    assignCoords :: EdgeList e -> [(V2 Float, V2 Float)] -> Int -> CoordList e
    assignCoords [] _ _ = []
    assignCoords (x@(a, _, _) : xs) (y@(left, right) : ys) start = if a == start then (x, y) : assignCoords xs ys start
                                                                   else (x, (right, left)) : assignCoords xs ys start
    assignCoords _ _ _ = error "Can not assign coords while restoring ends for cycle."

findAdjacentBondsCycles :: forall e. Eq e => CoordList e -> [CoordList e] -> Int -> Either (Coord e, Coord e) (V2 Float, V2 Float)
findAdjacentBondsCycles bondsOfCycle cycles ind = if length neighbors == 2 then Left (leftNeighbor, rightNeighbor)
                                                  else Right (beginning, beginning + bondLength *^ direction ^/ norm direction)
    where
      neighbors = findIncidentCoords ind bondsOfCycle

      [leftNeighbor, rightNeighbor] = take 2 neighbors

      cyclesInPlay = cleanListOfCoordLists (filter (\x -> any (`elem` x) neighbors) cycles) []
      beginning = findCommonVertexCoord leftNeighbor rightNeighbor
      direction = (beginning - centroid (head cyclesInPlay)) + (beginning - centroid (last cyclesInPlay))

      findCommonVertexCoord :: Coord e -> Coord e -> V2 Float
      findCommonVertexCoord ((a, b, _), (coordA, coordB)) ((a', b', _), _) | a == a' = coordA
                                                                           | a == b' = coordA
                                                                           | b == a' = coordB
                                                                           | otherwise = coordB
