module Math.Grads.Drawing.Coords
  ( BondFixator
  , Constraint (..)
  , CoordList
  , CoordMap
  , bondLength
  , getCoordsForGraph
  , getCoordsForGraphCons
  , getCoordsForGraphFix
  ) where

import           Control.Monad                                    (join)
import           Math.Grads.Algo.Cycles                           (findCycles)
import           Math.Grads.Algo.Interaction                      (getIndices)
import           Math.Grads.Drawing.Internal.Coords               (CoordList,
                                                                   CoordMap,
                                                                   bondLength,
                                                                   coordListForDrawing)
import           Math.Grads.Drawing.Internal.Cycles               (getCoordsOfGlobalCycle)
import           Math.Grads.Drawing.Internal.CyclesPathsAlignment (alignCyclesAndPaths)
import           Math.Grads.Drawing.Internal.Paths                (findPaths, getCoordsOfPath)
import           Math.Grads.Drawing.Internal.Sampling             (BondFixator, Constraint (..),
                                                                   bestSample,
                                                                   defBondFixator)
import           Math.Grads.GenericGraph                          (GenericGraph)
import           Math.Grads.Graph                                 (EdgeList,
                                                                   toList)
import           System.Random                                    (StdGen)

getCoordsForGraph :: (Ord v, Ord e, Eq e) => StdGen -> GenericGraph v e -> Maybe CoordMap
getCoordsForGraph = getCoordsForGraphCons []

getCoordsForGraphCons :: (Ord v, Ord e, Eq e) => [Constraint] -> StdGen -> GenericGraph v e -> Maybe CoordMap
getCoordsForGraphCons = getCoordsForGraphFix defBondFixator

getCoordsForGraphFix :: (Ord v, Ord e, Eq e) => BondFixator e -> [Constraint] -> StdGen -> GenericGraph v e -> Maybe CoordMap
getCoordsForGraphFix bondFixator constraints stdGen graph = res
  where
    (_, bonds) = toList graph
    (globalCycles, paths) = splitIntoCyclesAndPaths bonds

    globalCyclesWithCoords = sequence (fmap (getCoordsOfGlobalCycle pathsWithCoords) globalCycles)
    pathsWithCoords = fmap getCoordsOfPath paths

    finalCoords = join (fmap (alignCyclesAndPaths pathsWithCoords) globalCyclesWithCoords)
    resCoords = join (fmap (bestSample stdGen bondFixator constraints (concat paths)) finalCoords)

    res = fmap coordListForDrawing resCoords

splitIntoCyclesAndPaths :: (Ord e, Eq e) => EdgeList e -> ([EdgeList e], [EdgeList e])
splitIntoCyclesAndPaths bonds = (globalCycles, paths)
  where
    globalCycles = findCycles bonds
    forPaths = filter (`notElem` concat globalCycles) bonds
    paths = findPaths forPaths $ concatMap getIndices globalCycles
