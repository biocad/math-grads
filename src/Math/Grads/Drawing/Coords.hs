{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Math.Grads.Drawing.Coords
  ( BondFixator
  , Constraint (..)
  , CoordList
  , CoordMap
  , Drawable (..)
  , bondLength
  , getCoordsForGraph
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
                                                                   bestSample)
import           Math.Grads.GenericGraph                          (GenericGraph)
import           Math.Grads.Graph                                 (EdgeList,
                                                                   Graph,
                                                                   toList)
import           System.Random                                    (StdGen)

getCoordsForGraph :: (Ord v, Ord e, Eq e, Drawable GenericGraph v e) => StdGen -> GenericGraph v e -> Maybe CoordMap
getCoordsForGraph stdGen graph = res
  where
    (_, bonds) = toList graph
    (globalCycles, paths) = splitIntoCyclesAndPaths bonds

    globalCyclesWithCoords = sequence (fmap (getCoordsOfGlobalCycle pathsWithCoords) globalCycles)
    pathsWithCoords = fmap getCoordsOfPath paths

    finalCoords = join (fmap (alignCyclesAndPaths pathsWithCoords) globalCyclesWithCoords)
    resCoords = join (fmap (bestSample stdGen (bondFixator graph) (constraints graph) (concat paths)) finalCoords)

    res = fmap coordListForDrawing resCoords

splitIntoCyclesAndPaths :: (Ord e, Eq e) => EdgeList e -> ([EdgeList e], [EdgeList e])
splitIntoCyclesAndPaths bonds = (globalCycles, paths)
  where
    globalCycles = findCycles bonds
    forPaths = filter (`notElem` concat globalCycles) bonds
    paths = findPaths forPaths $ concatMap getIndices globalCycles

class Graph g => Drawable g v e where
  -- Change coordinates and fixate edges that shouldn't take part in sampling
  bondFixator :: g v e -> BondFixator e
  bondFixator _ = ((,) [])

  -- List of constraints for molecule
  constraints :: g v e -> [Constraint]
  constraints _ = []
