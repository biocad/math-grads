-- | Module that is responsible for linking systems of conjugated cycles in graph
-- with paths between them.
--
module Math.Grads.Drawing.Internal.CyclesPathsAlignment
  ( alignCyclesAndPaths
  , bondsToAlignTo
  , bondsToAlignToExtreme
  ) where

import           Control.Arrow                      (first, second, (***))
import           Data.Either                        (partitionEithers)
import           Data.List                          (find)
import           Data.Maybe                         (catMaybes, listToMaybe)
import           Linear.Matrix                      ((!*))
import           Linear.Metric                      (dot, norm)
import           Linear.V2                          (V2 (..))
import           Linear.Vector                      (negated, (*^))
import           Math.Grads.Algo.Paths              (findBeginnings)
import           Math.Grads.Angem                   (alignmentFunc, rotation2D)
import           Math.Grads.Drawing.Internal.Coords (bondLength)
import           Math.Grads.Drawing.Internal.Utils  (Coord, CoordList,
                                                     cleanCoordList,
                                                     tupleToList)
import           Math.Grads.Graph                   (EdgeList)

type CoordsEnds e = (CoordList e, EdgeList e)

-- | Given cycles and paths between them unites everything into one structure if possible.
--
alignCyclesAndPaths :: Eq e => [CoordList e] -> [CoordList e] -> Maybe (CoordList e)
alignCyclesAndPaths paths cycles = greedyAlignmentOfCyclesAndPaths (cyclesWithRestoredEnds ++ pathsWithRestoredEnds)
  where
    cyclesWithRestoredEnds = fmap linksForCycle cycles
    pathsWithRestoredEnds = fmap linksForPath paths

    linksForCycle :: CoordList e -> (CoordList e, EdgeList e)
    linksForCycle thisCycle = (thisCycle, findBondsToFind (fmap fst thisCycle))

    linksForPath :: CoordList e -> (CoordList e, EdgeList e)
    linksForPath thisPath = (thisPath, helper' (fmap fst thisPath))

    helper' :: EdgeList e -> EdgeList e
    helper' pathBondList = if length pathBondList == 1 then pathBondList
                           else findBondsToFind pathBondList

greedyAlignmentOfCyclesAndPaths :: Eq e => [(CoordList e, EdgeList e)] -> Maybe (CoordList e)
greedyAlignmentOfCyclesAndPaths [] = Nothing
greedyAlignmentOfCyclesAndPaths [x] = Just (fst x)
greedyAlignmentOfCyclesAndPaths (thisPart : otherParts) = if not (null toAdd) then res
                                                          else Nothing
  where
   theseCoords = fst thisPart
   bondsToFind = snd thisPart
   alignedNeighbors = fmap (detectAndAlignNeighbors bondsToFind theseCoords) otherParts

   (toAdd, leftParts) = first concat (partitionEithers alignedNeighbors)

   newTheseCoords = cleanCoordList (toAdd ++ theseCoords) []

   edgeList = fmap fst newTheseCoords
   newBondsToFind = findBondsToFind edgeList

   res = greedyAlignmentOfCyclesAndPaths ((newTheseCoords, newBondsToFind) : leftParts)

detectAndAlignNeighbors :: Eq e => EdgeList e -> CoordList e -> CoordsEnds e -> Either (CoordList e) (CoordsEnds e)
detectAndAlignNeighbors bondsToFind theseCoords theseCoordsEnds = maybe (Right theseCoordsEnds) Left neighsOrLeft
  where
    neighsOrLeft = detectAndAlignNeighborsM bondsToFind theseCoords theseCoordsEnds

detectAndAlignNeighborsM :: Eq e => EdgeList e -> CoordList e -> CoordsEnds e -> Maybe (CoordList e)
detectAndAlignNeighborsM bondsToFind theseCoords (coords, ends) = do
    let found' = catMaybes (fmap (\x -> find (== x) bondsToFind) ends)
    found <- listToMaybe found'

    let findBondToAlign = find (\(a, _) -> a == found)

    alignCoords <- coordToList <$> findBondToAlign theseCoords
    toAlignCoords <- coordToList <$> findBondToAlign coords

    let alignFunc = alignmentFunc alignCoords toAlignCoords

    Just (fmap (second (alignFunc *** alignFunc)) coords)
  where
    coordToList :: Coord e -> [V2 Float]
    coordToList = tupleToList . snd

findBondsToFind :: EdgeList e -> EdgeList e
findBondsToFind bonds = catMaybes ((\ind -> find (\(a, b, _) -> a == ind || b == ind) bonds) <$> findBeginnings bonds)

-- | Constructs edge that will be used to align to cycle containing given 'Coord's.
--
bondsToAlignTo :: Coord e -> Coord e -> Int -> [(V2 Float, V2 Float)]
bondsToAlignTo ((a, b, _), (pointA, pointB)) ((a', b', _), (pointA', pointB')) number = resultingVectors
  where
    coordA = pointB - pointA
    coordB = pointB' - pointA'
    ((vecA, vecB), linkingPoint) | a == a' = ((negated coordA, negated coordB), pointA)
                                 | a == b' = ((negated coordA, coordB), pointA)
                                 | b == a' = ((coordA, negated coordB), pointB)
                                 | otherwise = ((coordA, coordB), pointB)

    direction' = vecA + vecB
    direction = (bondLength / norm direction') *^ direction'
    toTopAngle = (180.0 - 180.0 * acos (dot vecA vecB / (norm vecA * norm vecB)) / pi) / 2.0
    angle' = 180.0 / fromIntegral number
    startingAngle = (180.0 - (fromIntegral number - 1.0) * angle') / 2.0

    dirA = dot (start (toTopAngle + startingAngle)) direction
    dirB = dot (start (-(toTopAngle + startingAngle))) direction
    startingPoint | dirA >= 0 && dirB >= 0 && dirA > dirB = start (toTopAngle + startingAngle)
                  | dirA >= 0 && dirB >= 0 = start (-(toTopAngle + startingAngle))
                  | dirA >= 0 = start (toTopAngle + startingAngle)
                  | otherwise = start (-(toTopAngle + startingAngle))

    mult = if dot (start (toTopAngle + startingAngle)) direction > 0 then 1 else (-1)
    resultingVectors = (\x -> (linkingPoint, linkingPoint + x)) <$> getDirections startingPoint 1 angle' number mult

    start :: Float -> V2 Float
    start angle = rotation2D angle !* ((bondLength / norm vecA) *^ negated vecA)

-- | If we have complicated situation where we need to calculate bonds to align to
-- for vertex in cycle that has more then 2 neighbors then we pass direction in
-- which we want to place neighbors and use bondsToAlignToExtreme function.
-- Otherwise we use bondsToAlignTo function.
--
bondsToAlignToExtreme :: (V2 Float, V2 Float) -> Int -> [(V2 Float, V2 Float)]
bondsToAlignToExtreme (beg, end) number = resultingVectors
  where
    direction = end - beg
    startingPointComplicated = rotation2D (-40.0) !* ((bondLength / norm direction) *^ direction)
    resultingVectors = (\x -> (beg, beg + x)) <$> getDirections startingPointComplicated 1 47.0 number 1

getDirections :: V2 Float -> Int -> Float -> Int -> Float -> [V2 Float]
getDirections prev counter angle number mult  = if counter < number then prev : getDirections new (counter + 1) angle number mult
                                                else [prev]
  where
    new = rotation2D (mult * angle) !* prev
