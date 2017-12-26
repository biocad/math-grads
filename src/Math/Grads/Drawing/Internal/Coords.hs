module Math.Grads.Drawing.Internal.Coords
  ( Coord
  , CoordList
  , CoordMap
  , Link
  , bondLength
  , coordListForDrawing
  , coordListToMap
  , coordMapToCoordList
  , tupleToList
  ) where

import           Control.Arrow                     ((***))
import           Data.List                         (sortOn)
import           Data.Map.Strict                   (Map, fromList, (!))
import           Linear.Metric                     (norm)
import           Linear.V2                         (V2 (..))
import           Linear.Vector                     ((^/))
import           Math.Angem                        (alignmentFunc, dist)
import           Math.Grads.Drawing.Internal.Utils (Coord, CoordList, pairToV2,
                                                    tupleToList, uV2)
import           Math.Grads.Graph                  (EdgeList, GraphEdge)

-- (Number of vertex, edge) for linked paths
type Link e = (Int, GraphEdge e)

-- Map that matches indexes of vertices to coordinates of these vertices
type CoordMap = Map Int (Float, Float)

bondLength :: Float
bondLength = 100.0

coordMapToCoordList :: CoordMap -> EdgeList e -> CoordList e
coordMapToCoordList coordMap = fmap (\bond@(a, b, _) -> (bond, (toV2Coord a, toV2Coord b)))
  where
    toV2Coord :: Int -> V2 Float
    toV2Coord = pairToV2 . (coordMap !)

coordListForDrawing :: Eq e => CoordList e -> CoordMap
coordListForDrawing coordinates = uV2 <$> coordListToMap coordsT
  where
    coordsT = rotateAlongLongestDist coordinates

coordListToMap :: Eq e => CoordList e -> Map Int (V2 Float)
coordListToMap coordinates = fromList (helper coordinates [] [])
  where

    helper :: CoordList e -> [Int] -> [(Int, V2 Float)] -> [(Int, V2 Float)]
    helper [] _ res = res
    helper (((a, b, _), (cA, cB)) : xs) taken res | a `elem` taken && b `elem` taken = helper xs taken res
                                                  | a `elem` taken && b `notElem` taken = helper xs (b : taken) ((b, cB) : res)
                                                  | a `notElem` taken && b `elem` taken = helper xs (a : taken) ((a, cA) : res)
                                                  | otherwise = helper xs (a : b : taken) ((a, cA) : (b, cB) : res)

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
    res = head (sortOn (\(a, b) -> -(dist a b)) (allPairs points))

    allPairs :: [a] -> [(a, a)]
    allPairs []       = []
    allPairs (x : xs) = fmap (\x' -> (x, x')) xs ++ allPairs xs
