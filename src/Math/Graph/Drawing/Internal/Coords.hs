module Math.Graph.Drawing.Internal.Coords
  ( Coord
  , CoordList
  , Link
  , bondLength
  , coordListForDrawing
  , findBond
  , findBondInd
  , tupleToList
  ) where

import           Control.Arrow                     ((***))
import           Data.List                         (find, findIndex, sortOn)
import           Data.Map.Strict                   (Map, fromList)
import           Linear.Metric                     (norm)
import           Linear.V2                         (V2 (..))
import           Linear.Vector                     ((^/))
import           Math.Angem                        (alignmentFunc, d2)
import           Math.Graph.Algo.Interaction       ((~=))
import           Math.Graph.Class                  (EdgeList, GraphEdge)
import           Math.Graph.Drawing.Internal.Utils (Coord, CoordList,
                                                    tupleToList, uV2)

-- (Number of atom, bond) for linked paths
type Link e = (Int, GraphEdge e)

bondLength :: Float
bondLength = 100.0

coordListForDrawing :: Eq e => CoordList e -> Map Int (Float, Float)
coordListForDrawing coordinates = fromList (helper coordsT [] [])
  where
    coordsT = rotateAlongLongestDist coordinates

    helper :: CoordList e -> [Int] -> [(Int, (Float, Float))] -> [(Int, (Float, Float))]
    helper [] _ res = res
    helper (((a, b, _), (cA, cB)) : xs) taken res | a `elem` taken && b `elem` taken = helper xs taken res
                                                  | a `elem` taken && b `notElem` taken = helper xs (b : taken) ((b, uV2 cB) : res)
                                                  | a `notElem` taken && b `elem` taken = helper xs (a : taken) ((a, uV2 cA) : res)
                                                  | otherwise = helper xs (a : b : taken) ((a, uV2 cA) : (b, uV2 cB) : res)

rotateAlongLongestDist :: CoordList e -> CoordList e
rotateAlongLongestDist coordinates = res
  where
    coordsU = getFloats coordinates
    (distA, distB) = findTwoMostDistantPoints coordsU
    dirVec = distB - distA

    alFunc = alignmentFunc [V2 0 0, V2 1 0] [V2 0 0, dirVec ^/ norm dirVec]
    res = fmap (alFunc *** alFunc) <$> coordinates

getFloats :: CoordList e -> [V2 Float]
getFloats coords = foldl (\x y -> x ++ tupleToList y) [] (fmap snd coords)

findTwoMostDistantPoints :: [V2 Float] -> (V2 Float, V2 Float)
findTwoMostDistantPoints points = res
  where
    res = head (sortOn (\(a, b) -> -(d2 a b)) (allPairs points))

    allPairs :: [a] -> [(a, a)]
    allPairs []       = []
    allPairs (x : xs) = fmap (\x' -> (x, x')) xs ++ allPairs xs

findBondInd :: EdgeList e -> Int -> Int -> Maybe Int
findBondInd bonds a b = found
  where
    leftA = min a b
    rightB = max a b

    found = findIndex (~= (leftA, rightB, undefined)) bonds

findBond :: CoordList e -> Int -> Int -> Maybe (Coord e)
findBond coords a b = found
  where
    leftA = min a b
    rightB = max a b

    found = find (\((a', b', _), _) -> a' == leftA && b' == rightB) coords
