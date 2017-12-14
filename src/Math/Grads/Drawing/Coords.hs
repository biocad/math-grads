module Math.Grads.Drawing.Coords
  ( Constraint (..)
  , CoordList
  , bondLength
  , getCoordsForGraph
  , getCoordsForGraphCons
  ) where

import           Control.Monad                                    (join)
import           Data.Map.Strict                                  (Map)
import           Math.Grads.Algo.Cycles                           (findCycles)
import           Math.Grads.Algo.Interaction                      (getIndices)
import           Math.Grads.Drawing.Internal.Coords               (CoordList,
                                                                   bondLength,
                                                                   coordListForDrawing)
import           Math.Grads.Drawing.Internal.Cycles               (getCoordsOfGlobalCycle)
import           Math.Grads.Drawing.Internal.CyclesPathsAlignment (alignCyclesAndPaths)
import           Math.Grads.Drawing.Internal.Paths                (findPaths, getCoordsOfPath)
import           Math.Grads.Drawing.Internal.Sampling             (Constraint (..),
                                                                   bestSample)
import           Math.Grads.GenericGraph                          (GenericGraph)
import           Math.Grads.Graph                                 (EdgeList,
                                                                   toList)
import           System.Random                                    (StdGen)

getCoordsForGraph :: (Ord v, Ord e, Eq e) => StdGen -> GenericGraph v e -> Maybe (Map Int (Float, Float))
getCoordsForGraph = getCoordsForGraphCons []

getCoordsForGraphCons :: (Ord v, Ord e, Eq e) => [Constraint] -> StdGen -> GenericGraph v e -> Maybe (Map Int (Float, Float))
getCoordsForGraphCons constraints stdGen graph = res
  where
    (_, bonds) = toList graph
    (globalCycles, paths) = splitIntoCyclesAndPaths bonds

    globalCyclesWithCoords = sequence (fmap (getCoordsOfGlobalCycle pathsWithCoords) globalCycles)
    pathsWithCoords = fmap getCoordsOfPath paths

    finalCoords = join (fmap (alignCyclesAndPaths pathsWithCoords) globalCyclesWithCoords)
    resCoords = join (fmap (bestSample stdGen constraints (concat paths)) finalCoords)

    res = fmap coordListForDrawing resCoords

splitIntoCyclesAndPaths :: (Ord e, Eq e) => EdgeList e -> ([EdgeList e], [EdgeList e])
splitIntoCyclesAndPaths bonds = (globalCycles, paths)
  where
    globalCycles = findCycles bonds
    forPaths = filter (`notElem` concat globalCycles) bonds
    paths = findPaths forPaths $ concatMap getIndices globalCycles
