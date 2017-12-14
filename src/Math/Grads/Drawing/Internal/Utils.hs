module Math.Grads.Drawing.Internal.Utils
  ( Coord
  , CoordList
  , randomVectors
  , findIncidentCoords
  , reflectCycle
  , reflectBond
  , centroid
  , tupleToList
  , compareCoords
  , cleanListOfCoordLists
  , cleanCoordList
  , coordElem
  , pairToV2
  , uV2
  ) where

import           Data.List                   (unfoldr)
import           Linear.V2                   (V2 (..))
import           Math.Angem                  (reflectPoint)
import           Math.Grads.Algo.Interaction (isIncident)
import           Math.Grads.Graph            (GraphEdge)
import           System.Random               (StdGen, randomR)

type CoordList e = [Coord e]
type Coord e = (GraphEdge e, (V2 Float, V2 Float))

randomVector :: Int -> StdGen -> ([Int], StdGen)
randomVector number gen = helper 0 ([], gen)
  where
    helper :: Int -> ([Int], StdGen) -> ([Int], StdGen)
    helper currentLength (a, g) =
      let
        (headA, newGen) = randomR (0, 1) g
      in if currentLength < number then helper (currentLength + 1) (headA : a, newGen)
         else (a, g)

randomVectors :: StdGen -> Int -> Int -> [[Int]]
randomVectors gen lengthOfVector numberOfVectors = helper gen
  where
    helper = take numberOfVectors . unfoldr (Just . randomVector lengthOfVector)

findIncidentCoords :: Int -> CoordList e -> CoordList e
findIncidentCoords ind = filter (flip isIncident ind . fst)

reflectCycle :: CoordList e -> (V2 Float, V2 Float) -> CoordList e
reflectCycle thisCycle ends = (`reflectBond` ends) <$> thisCycle

reflectBond :: Coord e -> (V2 Float, V2 Float) -> Coord e
reflectBond (bond, (bondCoordA, bondCoordB)) ends = (bond, (reflectPoint ends bondCoordA, reflectPoint ends bondCoordB))

centroid :: CoordList e -> V2 Float
centroid coords' = sum coords / fromIntegral (length coords)
  where
    coords = snd <$> foldl (\x ((a, b, _), (coordA, coordB)) -> helper x (a, coordA) (b, coordB)) [] coords'
    helper list (a, coordA') (b, coordB') | a `elem` fmap fst list && b `elem` fmap fst list = list
                                          | a `elem` fmap fst list = (b, coordB') : list
                                          | b `elem` fmap fst list = (a, coordA') : list
                                          | otherwise = (a, coordA') : ((b, coordB') : list)

tupleToList :: (a, a) -> [a]
tupleToList (x, y) = [x, y]

uV2 :: V2 Float -> (Float, Float)
uV2 (V2 a b) = (a, b)

pairToV2 :: (Float, Float) -> V2 Float
pairToV2 (a, b) = V2 a b

compareCoords :: Eq e => Coord e -> Coord e -> Bool
compareCoords (a, _) (b, _) = a == b

cleanListOfCoordLists :: Eq e => [CoordList e] -> [CoordList e] -> [CoordList e]
cleanListOfCoordLists [] final = final
cleanListOfCoordLists (x : xs) [] = cleanListOfCoordLists xs [x]
cleanListOfCoordLists (x : xs) final = if any (\thisCycle -> any (`coordElem` thisCycle) x) xs then cleanListOfCoordLists xs final
                                       else cleanListOfCoordLists xs (x : final)

cleanCoordList :: Eq e => CoordList e -> CoordList e -> CoordList e
cleanCoordList [] coords = coords
cleanCoordList (x : xs) coords = if not (coordElem x coords) then cleanCoordList xs (x : coords)
                                 else cleanCoordList xs coords

coordElem :: Eq e => Coord e -> CoordList e -> Bool
coordElem _ []         = False
coordElem coord coords = any (compareCoords coord) coords
