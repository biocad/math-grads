module Drawing.Internal.Sampling (bestSample) where

import           Data.List                  (delete, find)
import           Data.Maybe                 (fromJust)
import           System.Random              (StdGen)

import           Chemistry.Molecule         (Bond (..), BondList, MolBond)
import           Drawing.Internal.Utilities (Coord, CoordList, randomVectors,
                                             reflectBond)
import           Graph.Paths                (findBeginnings)
import           Graph.Traversals           (dfs)
import           Math.Math                  (areIntersected, myEqV2)

-- Find conformation with minimal number of intersections
bestSample :: CoordList -> BondList -> StdGen -> Maybe CoordList
bestSample coords bondsOfPaths stdGen = if findIntersections resSample /= 0 then Nothing
                                        else Just resSample
  where
    begs = findBeginnings (fmap fst coords)
    rotatableBonds = filter (\(a, b, t) -> t == Single && not (a `elem` begs || b `elem` begs)) bondsOfPaths

    samples = generateSamples coords rotatableBonds stdGen
    curInt = findIntersections (head samples)

    resSample = if curInt == 0 then head samples
                else minInterSample (tail samples) (head samples) curInt

minInterSample :: [CoordList] -> CoordList -> Int -> CoordList
minInterSample [] prev _ = prev
minInterSample (x : xs) prev prevMin | curInt' >= prevMin = minInterSample xs prev prevMin
                                     | curInt' == 0 = x
                                     | otherwise = minInterSample xs x curInt'
  where
    curInt' = findIntersections x

generateSamples :: CoordList -> BondList -> StdGen -> [CoordList]
generateSamples coords [] _ = [coords]
generateSamples coords rotatableBonds stdGen = (rotateOnBonds coords <$>) filteredSubsets
  where
    numberOfSamples = 2000
    lengthOfBonds = length rotatableBonds
    vectors = replicate lengthOfBonds 0 : randomVectors stdGen lengthOfBonds numberOfSamples
    filteredSubsets = fmap (\vector -> concatMap (\(x, y) -> [y | x == 1]) (zip vector rotatableBonds)) vectors

rotateOnBonds :: CoordList -> BondList -> CoordList
rotateOnBonds = foldl rotateOnBond

rotateOnBond :: CoordList -> MolBond -> CoordList
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

doOverlap :: Coord -> Coord -> Bool
doOverlap ((a, b, _), (coordA, coordB)) ((a', b', _), (coordA', coordB')) = condA || condB
  where
    condA = coordA `myEqV2` coordA' && coordB `myEqV2` coordB' ||
      coordA `myEqV2` coordB' && coordB `myEqV2` coordA'
    condB = a /= a' && coordA `myEqV2` coordA' || a /= b' && coordA `myEqV2` coordB' ||
      b /= a' && coordB `myEqV2` coordA' || b /= b' && coordB `myEqV2` coordB'

findIntersections :: CoordList -> Int
findIntersections = helper
   where
     helper :: CoordList -> Int
     helper []       = error "Find intersections helper on empty list."
     helper [_]      = 0
     helper (x : xs) = foldl (allLeftIntersections x) 0 xs + helper xs

allLeftIntersections :: Coord -> Int -> Coord -> Int
allLeftIntersections coord x' coord' = x' + addIfIntersect coord coord'

addIfIntersect :: Coord -> Coord -> Int
addIfIntersect x@(bond, coords) coord@(bond', coords') = if cond then 1 else 0
  where
    cond = bond /= bond' && (doOverlap x coord || areIntersected coords coords')
