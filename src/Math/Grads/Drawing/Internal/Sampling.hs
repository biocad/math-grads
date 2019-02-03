{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Module that provides function for sampling of coords of graph.
--
module Math.Grads.Drawing.Internal.Sampling
  ( EdgeFixator
  , bestSample
  ) where

import           Data.List                          (delete, find, (\\))
import           Data.Maybe                         (fromJust)
import           Math.Grads.Algo.Traversals         (dfs)
import           Math.Grads.Angem                   (areIntersected, eqV2)
import           Math.Grads.Drawing.Internal.Coords (CoordMap,
                                                     coordListForDrawing,
                                                     coordMapToCoordList)
import           Math.Grads.Drawing.Internal.Utils  (Coord, CoordList,
                                                     randomVectors, reflectBond)
import           Math.Grads.Graph                   (EdgeList, GraphEdge)
import           System.Random                      (StdGen)

-- | Type alias for function that, given 'CoordMap' of graph, returns modified
-- version of that 'CoordMap' alongside with 'EdgeList' of graph's edges of that
-- shouldn't participate in sampling.
--
type EdgeFixator e = CoordMap -> (EdgeList e, CoordMap)

-- | Finds conformation with minimal number of intersections.
--
bestSample :: Eq e => StdGen -> EdgeFixator e -> EdgeList e -> CoordList e -> Maybe (CoordList e)
bestSample stdGen edgeFixator bondsOfPaths coords = res
  where
    (fixedBonds, coordsChangedMap) = edgeFixator (coordListForDrawing coords)

    coordsChanged = coordMapToCoordList coordsChangedMap (fmap fst coords)
    samples       = generateSamples stdGen coordsChanged (bondsOfPaths \\ fixedBonds)
    curInt        = findIntersections (head samples)

    resSample = if curInt == 0 then head samples
                else minInterSample (tail samples) (head samples) curInt

    res = if findIntersections resSample /= 0 then Nothing
          else Just resSample

minInterSample :: Eq e => [CoordList e] -> CoordList e -> Int -> CoordList e
minInterSample [] prev _ = prev
minInterSample (x : xs) prev prevMin | curInt' >= prevMin = minInterSample xs prev prevMin
                                     | curInt' == 0       = x
                                     | otherwise          = minInterSample xs x curInt'
  where
    curInt' = findIntersections x

generateSamples :: Eq e => StdGen -> CoordList e -> EdgeList e -> [CoordList e]
generateSamples _ coords [] = [coords]
generateSamples stdGen coords rotatableBonds = (rotateOnBonds coords <$>) filteredSubsets
  where
    numberOfSamples = 2000
    lengthOfBonds   = length rotatableBonds

    vectors         = replicate lengthOfBonds 0 : randomVectors stdGen lengthOfBonds numberOfSamples
    filteredSubsets = fmap (\vector -> concatMap (\(x, y) -> [y | x == 1]) (zip vector rotatableBonds)) vectors

rotateOnBonds :: Eq e => CoordList e -> EdgeList e -> CoordList e
rotateOnBonds = foldl rotateOnBond

rotateOnBond :: Eq e => CoordList e -> GraphEdge e -> CoordList e
rotateOnBond coords bond = res
  where
    bondItself@((_, b, _), (coordA, coordB)) = fromJust (find ((== bond) . fst) coords)

    toTheRightBonds = dfs (fst <$> delete bondItself coords) b

    toTheRightCoords = filter (\(x, _) -> x `elem` toTheRightBonds) coords
    toTheLeftCoords  = filter (\(x, _) -> notElem x toTheRightBonds) coords

    (doNotRotate, rotate) = if length toTheLeftCoords < length toTheRightCoords then (toTheRightCoords, toTheLeftCoords)
                            else (toTheLeftCoords, toTheRightCoords)

    res = if null toTheLeftCoords || null toTheRightCoords then coords
          else doNotRotate ++ ((`reflectBond` (coordA, coordB)) <$> rotate)

doOverlap :: Coord e -> Coord e -> Bool
doOverlap ((a, b, _), (coordA, coordB)) ((a', b', _), (coordA', coordB')) = condA || condB
  where
    condA = coordA `eqV2` coordA' && coordB `eqV2` coordB' ||
      coordA `eqV2` coordB' && coordB `eqV2` coordA'
    condB = a /= a' && coordA `eqV2` coordA' || a /= b' && coordA `eqV2` coordB' ||
      b /= a' && coordB `eqV2` coordA' || b /= b' && coordB `eqV2` coordB'

findIntersections :: forall e. Eq e => CoordList e -> Int
findIntersections []       = error "Find intersections helper on empty list."
findIntersections [_]      = 0
findIntersections (x : xs) = foldl (allLeftIntersections x) 0 xs + findIntersections xs

allLeftIntersections :: Eq e => Coord e -> Int -> Coord e -> Int
allLeftIntersections coord x' coord' = x' + addIfIntersect coord coord'

addIfIntersect :: Eq e => Coord e -> Coord e -> Int
addIfIntersect x@(bond, coords) coord@(bond', coords') = fromEnum cond
  where
    cond = bond /= bond' && (doOverlap x coord || areIntersected coords coords')
