{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Module providing functions for obtaining coordinates of 'GenericGraph's on
-- a 2D plane.
--
module Math.Grads.Drawing.Coords
  ( Coord
  , CoordList
  , CoordMap
  , Drawable (..)
  , EdgeFixator
  , bondLength
  , getCoordsForGraph
  ) where

import           Control.Monad                                    (join)
import           Data.Map.Strict                                  (keys,
                                                                   singleton)
import           Math.Grads.Algo.Cycles                           (findCycles)
import           Math.Grads.Algo.Interaction                      (getIndices)
import           Math.Grads.Drawing.Internal.Coords               (Coord,
                                                                   CoordList,
                                                                   CoordMap,
                                                                   bondLength,
                                                                   coordListForDrawing)
import           Math.Grads.Drawing.Internal.Cycles               (getCoordsOfGlobalCycle)
import           Math.Grads.Drawing.Internal.CyclesPathsAlignment (alignCyclesAndPaths)
import           Math.Grads.Drawing.Internal.Paths                (findPaths, getCoordsOfPath)
import           Math.Grads.Drawing.Internal.Sampling             (EdgeFixator,
                                                                   bestSample)
import           Math.Grads.GenericGraph                          (GenericGraph)
import           Math.Grads.Graph                                 (EdgeList,
                                                                   Graph,
                                                                   toList)
import           System.Random                                    (StdGen)

-- | Type class that defines whether graph can be drawn or not.
--
class Graph g => Drawable g v e where
  -- | Change coordinates and fixate edges that shouldn't take part in sampling.
  --
  edgeFixator :: g v e -> EdgeFixator e
  edgeFixator = const $ (,) []

-- | Given 'StdGen' returns 'CoordMap', which keys correspond to indices of
-- vertices of given 'GenericGraph'. Works only for simple planar graphs. If graph
-- is neither simple nor planar, returns Nothing. This function is best used for
-- graphs that can be represented as systems of conjugated cycles and paths between
-- them. If graph contains too complex conjugated cycles, function will return Nothing.
--
getCoordsForGraph :: (Ord v, Ord e, Eq e, Drawable GenericGraph v e) => StdGen -> GenericGraph v e -> Maybe CoordMap
getCoordsForGraph stdGen graph = if length vertices == 1 then Just (singleton 0 (0, 0))
                                 else res
  where
    (vertices, edges)        = toList graph
    (globalCycles, paths) = splitIntoCyclesAndPaths edges

    globalCyclesWithCoords = sequence (fmap (getCoordsOfGlobalCycle pathsWithCoords) globalCycles)
    pathsWithCoords        = fmap getCoordsOfPath paths

    finalCoords = join (fmap (alignCyclesAndPaths pathsWithCoords) globalCyclesWithCoords)
    resCoords   = join (fmap (bestSample stdGen (edgeFixator graph) (concat paths)) finalCoords)

    resMap = fmap coordListForDrawing resCoords

    res = if fmap (length . keys) resMap == pure (length vertices) then resMap else Nothing

splitIntoCyclesAndPaths :: (Ord e, Eq e) => EdgeList e -> ([EdgeList e], [EdgeList e])
splitIntoCyclesAndPaths edges = (globalCycles, paths)
  where
    globalCycles = findCycles edges
    forPaths = filter (`notElem` concat globalCycles) edges
    paths = findPaths forPaths $ concatMap getIndices globalCycles
