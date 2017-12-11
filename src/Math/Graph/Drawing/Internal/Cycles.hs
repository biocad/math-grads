module Math.Graph.Drawing.Internal.Cycles (getCoordsOfGlobalCycle) where

import           Data.Either                           (isLeft)
import           Data.Either.Combinators               (fromLeft', fromRight')
import           Data.List                             (find, groupBy, nub,
                                                        sortOn)
import           Data.Maybe                            (fromJust, isJust,
                                                        mapMaybe)
import           Linear.Metric                         (norm)
import           Linear.V2                             (V2 (..))
import           Linear.Vector                         ((*^), (^/))

import qualified Data.Array                            as A
import qualified Data.Map.Strict                       as M
import           Drawing.Internal.CyclesPathsAlignment (bondsToAlignTo,
                                                        bondsToAlignToExtreme)
import           Drawing.Internal.Utilities            (Coord, CoordList,
                                                        centroid,
                                                        cleanCoordList,
                                                        cleanListOfCoordLists,
                                                        compareCoords,
                                                        findIncidentCoords,
                                                        reflectCycle,
                                                        tupleToList)
import           Graph.Class                           (fromList, vCount)
import           Graph.Cycles                          (findLocalCycles)
import           Graph.EdgeVertexUtils                 (getEnds, getIndices)
import           Graph.GenericGraph                    (gAdjacency, gIndex)
import           Graph.Paths                           (findBeginnings)
import           Graph.Traversals                      (dfsCycle)
import           Graph.Utils                           (uniter)
import           Math.Graph.Class                      (EdgeList, GraphEdge)
import           Math.Graph.Drawing.Internal.Meta      (Link, bondLength)
import           Math.Math                             (alignmentFunc, d2)

-- Calculates coordinates of system of cycles and coordinates of bonds that are adjacent to it
getCoordsOfGlobalCycle :: [CoordList] -> BondList -> (CoordList, [BondList])
getCoordsOfGlobalCycle paths globalCycle = (restoreEndsForCycle res paths aligned, localCycles)
  where
    localCycles = findLocalCycles globalCycle
    localCyclesWithCoords = sortOn (\x -> - (length x)) (fmap getCoordsOfLocalCycle localCycles)
    aligned = greedyAlignmentOfLocalCycles [head localCyclesWithCoords] (tail localCyclesWithCoords)
    res = cleanCoordList (concat aligned) []

getCoordsOfLocalCycle :: BondList -> CoordList
getCoordsOfLocalCycle thisCycle = matchBonds thisCycle (getCoordsOfPolygon (length thisCycle))

getCoordsOfPolygon :: Int -> [(V2 Float, V2 Float)]
getCoordsOfPolygon number = let coords = fmap getPoint [0..number - 1] in (last coords, head coords) : uniter coords
  where
    angle = 2 * pi / fromIntegral number
    radius = bondLength / sin (angle / 2) / 2

    getPoint :: Int -> V2 Float
    getPoint step = V2 (radius * cos (fromIntegral step * angle)) (radius * sin (fromIntegral step * angle))

uniteLocalCyclesOnBond :: Coord -> Coord -> CoordList -> CoordList
uniteLocalCyclesOnBond (_, coords) (_, coords') toTransformCoords = transformFuncCoord <$> toTransformCoords
  where
    transformFunc' = alignmentFunc (tupleToList coords) (tupleToList coords')
    transformFuncCoord (bond, (a, b)) = (bond, (transformFunc' a, transformFunc' b))

matchBonds :: BondList -> [(V2 Float, V2 Float)] -> CoordList
matchBonds bonds coords = matchBonds' bonds (zip bondsInd coords)
  where
    vertices = nub $ concatMap getEnds bonds
    index = M.fromList (zip vertices [0..])
    graph = fromList (vertices, fmap (\(a, b, t) -> (index M.! a, index M.! b, t)) bonds)
    graphArray = fmap fst <$> gAdjacency graph

    inds = (gIndex graph A.!) <$> dfsCycle graphArray [0 .. (vCount graph - 1)] []
    -- inds = dfs'
    bondsInd = uniter inds ++ [(last inds, head inds)]

matchBonds' :: BondList -> [((Int, Int), (V2 Float, V2 Float))] -> CoordList
matchBonds' bonds match = fmap (changeCoords match) bonds

changeCoords :: [((Int, Int), (V2 Float, V2 Float))] -> MolBond -> Coord
changeCoords [] _ = error "Change coords on empty list."
changeCoords (((a', b'), (left, right)) : xs) bond@(a, b, _) | a == a' && b == b' = (bond, (left, right))
                                                             | a == b' && b == a' = (bond, (right, left))
                                                             | otherwise = changeCoords xs bond

greedyAlignmentOfLocalCycles :: [CoordList] -> [CoordList] -> [CoordList]
greedyAlignmentOfLocalCycles mainCycles [] = mainCycles
greedyAlignmentOfLocalCycles mainCycles xs = res
  where
    neighborExists = fmap checkForNeighbor xs
    idOfNeighbor = helper neighborExists 0
    neighbor = xs !! idOfNeighbor

    x = concat mainCycles
    matches = filter isJust (concatMap findMatchingBond x)
    (coordsA, coordsB) = fromJust (head matches)

    reflectedIfNeeded = reflectIfIntersects (uniteLocalCyclesOnBond coordsA coordsB neighbor) mainCycles (snd coordsA)
    finalCycle = correctLeftMatches ((snd . fromJust) <$> tail matches) reflectedIfNeeded x

    res = greedyAlignmentOfLocalCycles (finalCycle : mainCycles) (take idOfNeighbor xs ++ drop (idOfNeighbor + 1) xs)

    findMatchingBond :: Coord -> [Maybe (Coord, Coord)]
    findMatchingBond thisBond  = fmap (hasMatch thisBond) neighbor

    hasMatch :: Coord -> Coord -> Maybe (Coord, Coord)
    hasMatch thisBond otherBond = if compareCoords thisBond otherBond then Just (thisBond, otherBond)
                                  else Nothing

    checkForNeighbor :: CoordList -> Bool
    checkForNeighbor = any (\otherCoord -> any (compareCoords otherCoord) x)

    helper :: [Bool] -> Int -> Int
    helper [] _           = error "Greedy alignment of cycles helper on empty list."
    helper (y : ys) counter = if y then counter else helper ys (counter + 1)

reflectIfIntersects :: CoordList -> [CoordList] -> (V2 Float, V2 Float) -> CoordList
reflectIfIntersects thisCycle allCycles (coordA, coordB) = if intersects then reflectCycle thisCycle (coordA, coordB)
                                                           else thisCycle
  where
    thisCentroid = centroid thisCycle
    otherCentroids = centroid <$> allCycles
    intersects = any (\x -> sqrt (d2 x thisCentroid) <= bondLength) otherCentroids

correctLeftMatches :: [Coord] -> CoordList -> CoordList -> CoordList
correctLeftMatches [] thisCycle _ = thisCycle
correctLeftMatches ((bond@(beg, end, _), _) : xs) thisCycle mainCycles = correctLeftMatches xs thisCycleUpdated mainCycles
  where
    thisCycleUpdated = fromJust <$> filter isJust (fmap correctMatch thisCycle)

    correctMatch :: Coord -> Maybe Coord
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

    helper :: Int -> Coord -> Maybe (V2 Float)
    helper endToFix ((a', b', _), (coordA', coordB')) | a' == endToFix = Just coordA'
                                                      | b' == endToFix = Just coordB'
                                                      | otherwise = Nothing

restoreEndsForCycle :: CoordList -> [CoordList] -> [CoordList] -> CoordList
restoreEndsForCycle thisCycle [[]] _ = thisCycle
restoreEndsForCycle thisCycle paths localCycles = thisCycle ++ concat neighbors
  where
    verticesOfCycle = getIndices (fmap fst thisCycle)
    cycleLinkingCoords = mapMaybe (findLinks verticesOfCycle) paths
    counted = countNeighbors' cycleLinkingCoords
    neighbors = fmap (getLinksWithCoords thisCycle localCycles) counted

countNeighbors' :: [(Int, MolBond)] -> [(Int, BondList)]
countNeighbors' list = (\x -> let (a, b) = unzip x in (head a, b)) <$> groupBy (\a b -> fst a == fst b) list

findLinks :: [Int] -> CoordList -> Maybe Link
findLinks verticesOfCycle path = if not (null found) then Just (foundVertex, fst (fromJust bond))
                                 else Nothing
  where
    found = filter (`elem` verticesOfCycle) (findBeginnings (fmap fst path))
    foundVertex = head found
    bond = find (\((a, b, _), _) -> a == foundVertex || b == foundVertex) path

getLinksWithCoords :: CoordList -> [CoordList] -> (Int, BondList) -> CoordList
getLinksWithCoords thisCycle localCycles (ind, bonds) = res
  where
    found = findAdjacentBondsCycles thisCycle localCycles ind
    direction = fromRight' found
    (leftBond, rightBond, extreme) = if isLeft found then (fst (fromLeft' found), snd (fromLeft' found), False)
                                     else (head thisCycle, head thisCycle, True)

    alignToFunc = if extreme then bondsToAlignToExtreme direction else bondsToAlignTo
    res = assignCoords bonds (alignToFunc leftBond rightBond (length bonds)) ind

    assignCoords :: BondList -> [(V2 Float, V2 Float)] -> Int -> CoordList
    assignCoords [] _ _ = []
    assignCoords (x@(a, _, _) : xs) (y@(left, right) : ys) start = if a == start then (x, y) : assignCoords xs ys start
                                                                   else (x, (right, left)) : assignCoords xs ys start
    assignCoords _ _ _ = error "Can not assign coords while restoring ends for cycle."

findAdjacentBondsCycles :: CoordList -> [CoordList] -> Int -> Either (Coord, Coord) (V2 Float, V2 Float)
findAdjacentBondsCycles bondsOfCycle cycles ind = if length neighbors == 2 then Left (leftNeighbor, rightNeighbor)
                                                  else Right (beginning, beginning + bondLength *^ direction ^/ norm direction)
    where
      neighbors = findIncidentCoords ind bondsOfCycle

      [leftNeighbor, rightNeighbor] = take 2 neighbors

      cyclesInPlay = cleanListOfCoordLists (filter (\x -> any (`elem` x) neighbors) cycles) []
      beginning = findCommonVertexCoord leftNeighbor rightNeighbor
      direction = (beginning - centroid (head cyclesInPlay)) + (beginning - centroid (last cyclesInPlay))

      findCommonVertexCoord :: Coord -> Coord -> V2 Float
      findCommonVertexCoord ((a, b, _), (coordA, coordB)) ((a', b', _), _) | a == a' = coordA
                                                                           | a == b' = coordA
                                                                           | b == a' = coordB
                                                                           | otherwise = coordB
