module Drawing.Coords (CoordList,
                       DirIndList,
                       DirInd,
                       coordListForDrawing,
                       getCoordsOfMol,
                       getCoordsOfMolSmarts
                       ) where

import           System.Random                         (StdGen)

import           Chemistry.Molecule                    (BondList, Molecule (..))
import           Data.Map.Strict                       (Map)
import           Data.Maybe                            (fromJust, isJust)
import           Drawing.Internal.Coords               (CoordList,
                                                        coordListForDrawing)
import           Drawing.Internal.Cycles               (getCoordsOfGlobalCycle)
import           Drawing.Internal.CyclesPathsAlignment (alignCyclesAndPaths)
import           Drawing.Internal.Isomerism            (DirInd, DirIndList,
                                                        fixIso, getTetra)
import           Drawing.Internal.Paths                (findPaths,
                                                        getCoordsOfPath)
import           Drawing.Internal.Sampling             (bestSample)
import           Graph.Cycles                          (findCycles,
                                                        findLocalCycles)
import           Graph.EdgeVertexUtils                 (getIndices)

-- Given molecule and random numbers generator calculate coordinates of molecules's atoms
getCoordsOfMol :: Molecule -> StdGen -> (CoordList, DirIndList)
getCoordsOfMol mol stdGen = if isJust beforeTetra then getTetra mol (fromJust beforeTetra)
                            else error "Can't draw the molecule given."
  where
    (firstCoords, paths, _) = firstCoordsForMol mol
    beforeSampling = fixIso mol firstCoords

    beforeTetra = bestSample beforeSampling (fst <$> concat paths) stdGen

splitIntoCyclesAndPaths :: BondList -> ([BondList], [BondList])
splitIntoCyclesAndPaths bonds = (globalCycles, paths)
  where
    globalCycles = findCycles bonds
    forPaths = filter (`notElem` concat globalCycles) bonds
    paths = findPaths forPaths $ concatMap getIndices globalCycles

getCoordsOfMolSmarts :: Molecule -> StdGen -> (Map Int (Float, Float), [[Int]])
getCoordsOfMolSmarts smartsMol stdGen = if isJust resCoords then res
                                        else error "Can't draw the molecule given."
  where
    (firstCoords, paths, globalCycles) = firstCoordsForMol smartsMol

    resCoords = bestSample firstCoords (fst <$> concat paths) stdGen

    allCycles = getIndices <$> concatMap findLocalCycles globalCycles
    aromaticAtoms = getIndices (getAromBonds smartsMol)

    arCycles = filter (all (`elem` aromaticAtoms)) allCycles

    coordMap = coordListForDrawing (fromJust resCoords)

    res = (coordMap, arCycles)

firstCoordsForMol :: Molecule -> (CoordList, [CoordList], [BondList])
firstCoordsForMol molecule = res
  where
    bonds = getBonds molecule
    (globalCycles, paths) = splitIntoCyclesAndPaths bonds

    (globalCyclesWithCoords, toConcat) = unzip (fmap (getCoordsOfGlobalCycle pathsWithCoords) globalCycles)
    localCycles = concat toConcat
    pathsWithCoords = fmap getCoordsOfPath paths

    finalCoords = alignCyclesAndPaths globalCyclesWithCoords pathsWithCoords
    res = (finalCoords, pathsWithCoords, localCycles)
