{-# LANGUAGE ScopedTypeVariables #-}

module Math.Grads.Drawing.Internal.Sampling (Constraint (..), bestSample) where

import           Data.List                          (delete, find)
import           Data.Map.Strict                    (Map, (!))
import           Data.Maybe                         (fromJust)
import           Linear.V2                          (V2)
import           Math.Angem                         (areIntersected, d2, eqV2)
import           Math.Grads.Algo.Traversals         (dfs)
import           Math.Grads.Drawing.Internal.Coords (coordListToMap)
import           Math.Grads.Drawing.Internal.Utils  (Coord, CoordList,
                                                     randomVectors, reflectBond)
import           Math.Grads.Graph                   (EdgeList, GraphEdge)
import           System.Random                      (StdGen)

data Constraint = VPair { vPair :: (Int -> V2 Float) -> Bool
                        }

-- Find conformation with minimal number of intersections
bestSample :: Eq e => StdGen -> [Constraint] -> EdgeList e -> CoordList e -> Maybe (CoordList e)
bestSample stdGen constraints bondsOfPaths coords = if findIntersections constraints resSample /= 0 then Nothing
                                                    else Just resSample
  where
    samples = generateSamples stdGen coords bondsOfPaths
    curInt = findIntersections constraints (head samples)

    resSample = if curInt == 0 then head samples
                else minInterSample constraints (tail samples) (head samples) curInt

minInterSample :: Eq e => [Constraint] -> [CoordList e] -> CoordList e -> Int -> CoordList e
minInterSample _ [] prev _ = prev
minInterSample constraints (x : xs) prev prevMin | curInt' >= prevMin = minInterSample constraints xs prev prevMin
                                                 | curInt' == 0 = x
                                                 | otherwise = minInterSample constraints xs x curInt'
  where
    curInt' = findIntersections constraints x

generateSamples :: Eq e => StdGen -> CoordList e -> EdgeList e -> [CoordList e]
generateSamples _ coords [] = [coords]
generateSamples stdGen coords rotatableBonds = (rotateOnBonds coords <$>) filteredSubsets
  where
    numberOfSamples = 2000
    lengthOfBonds = length rotatableBonds
    vectors = replicate lengthOfBonds 0 : randomVectors stdGen lengthOfBonds numberOfSamples
    filteredSubsets = fmap (\vector -> concatMap (\(x, y) -> [y | x == 1]) (zip vector rotatableBonds)) vectors

rotateOnBonds :: Eq e => CoordList e -> EdgeList e -> CoordList e
rotateOnBonds = foldl rotateOnBond

rotateOnBond :: Eq e => CoordList e -> GraphEdge e -> CoordList e
rotateOnBond coords bond = res
  where
    bondItself@((_, b, _), (coordA, coordB)) = fromJust (find (\(bond', _) -> bond' == bond) coords)
    toTheRightBonds = dfs (fst <$> delete bondItself coords) b
    toTheRightCoords = filter (\(x, _) -> x `elem` toTheRightBonds) coords
    toTheLeftCoords = filter (\(x, _) -> notElem x toTheRightBonds) coords

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

findIntersections :: forall e. Eq e => [Constraint] -> CoordList e -> Int
findIntersections constraints coords = addConstraints coords constraints + helper coords
   where
     helper :: CoordList e -> Int
     helper []       = error "Find intersections helper on empty list."
     helper [_]      = 0
     helper (x : xs) = foldl (allLeftIntersections x) 0 xs + helper xs

allLeftIntersections :: Eq e => Coord e -> Int -> Coord e -> Int
allLeftIntersections coord x' coord' = x' + addIfIntersect coord coord'

addIfIntersect :: Eq e => Coord e -> Coord e -> Int
addIfIntersect x@(bond, coords) coord@(bond', coords') = fromEnum cond
  where
    cond = bond /= bond' && (doOverlap x coord || areIntersected coords coords')

addConstraints :: Eq e => CoordList e -> [Constraint] -> Int
addConstraints coords = sum . fmap (\x -> fromEnum (vPair x (coordsMap !)))
  where
    coordsMap = coordListToMap coords
