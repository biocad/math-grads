-- | Module that provides utility functions for whole Drawing module.
--
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

import           Control.Arrow               ((***))
import           Data.List                   (unfoldr)
import           Linear.V2                   (V2 (..))
import           Math.Grads.Algo.Interaction (isIncident)
import           Math.Grads.Angem            (reflectPoint)
import           Math.Grads.Graph            (GraphEdge)
import           System.Random               (StdGen, randomR)

-- | Alias for list of 'Coord's.
--
type CoordList e = [Coord e]

-- | 'Coord' is pair that containts graph's edge and coordinates of vertices that
-- are incident to it.
--
type Coord e = (GraphEdge e, (V2 Float, V2 Float))

-- | Generates vector of random 'Int's of given length.
--
randomVector :: Int -> StdGen -> ([Int], StdGen)
randomVector len gen = helper 0 ([], gen)
  where
    helper :: Int -> ([Int], StdGen) -> ([Int], StdGen)
    helper currentLength (a, g) | currentLength < len = helper (currentLength + 1) (headA : a, newGen)
                                | otherwise           = (a, g)
      where
        (headA, newGen) = randomR (0, 1) g

-- | Generates list of random vectors of length numberOfVectors.
-- Each vector has length lengthOfVector.
--
randomVectors :: StdGen -> Int -> Int -> [[Int]]
randomVectors gen lengthOfVector numberOfVectors = helper gen
  where
    helper = take numberOfVectors . unfoldr (Just . randomVector lengthOfVector)

-- | Find all 'Coord's in 'CoordList' that are incident to vertex with given index.
--
findIncidentCoords :: Int -> CoordList e -> CoordList e
findIncidentCoords ind = filter (flip isIncident ind . fst)

-- | Reflect given cycle in form of 'CoordList' over given axis.
--
reflectCycle :: CoordList e -> (V2 Float, V2 Float) -> CoordList e
reflectCycle thisCycle = (<$> thisCycle) . flip reflectBond

-- | Reflect given 'Coord' over given axis.
--
reflectBond :: Coord e -> (V2 Float, V2 Float) -> Coord e
reflectBond coord ends = fmap (reflectPoint ends *** reflectPoint ends) coord

-- | Calculates centroid of vertices in given 'CoordList'.
--
centroid :: CoordList e -> V2 Float
centroid coords' = sum coords / fromIntegral (length coords)
  where
    coords = snd <$> foldl (\x ((a, b, _), (coordA, coordB)) -> helper x (a, coordA) (b, coordB)) [] coords'
    helper list (a, coordA') (b, coordB') | a `elem` fmap fst list && b `elem` fmap fst list = list
                                          | a `elem` fmap fst list = (b, coordB') : list
                                          | b `elem` fmap fst list = (a, coordA') : list
                                          | otherwise = (a, coordA') : ((b, coordB') : list)

-- | Converts tuple to lis.
--
tupleToList :: (a, a) -> [a]
tupleToList (x, y) = [x, y]

-- | Converts 'V2' to pair.
--
uV2 :: V2 Float -> (Float, Float)
uV2 (V2 a b) = (a, b)

-- | Converts pair to 'V2'.
--
pairToV2 :: (Float, Float) -> V2 Float
pairToV2 (a, b) = V2 a b

-- | Given 'CoordList' of conjugated cycles leaves only cycles that don't intersect
-- with each other excluding first cycle in list that is taken by default.
--
cleanListOfCoordLists :: Eq e => [CoordList e] -> [CoordList e] -> [CoordList e]
cleanListOfCoordLists [] final       = final
cleanListOfCoordLists (x : xs) []    = cleanListOfCoordLists xs [x]
cleanListOfCoordLists (x : xs) final = if any (\thisCycle -> any (`coordElem` thisCycle) x) xs then cleanListOfCoordLists xs final
                                       else cleanListOfCoordLists xs (x : final)

-- | Leaves only unique 'Coord's in given 'CoordList'.
--
cleanCoordList :: Eq e => CoordList e -> CoordList e -> CoordList e
cleanCoordList [] coords       = coords
cleanCoordList (x : xs) coords = if not (x `coordElem` coords) then cleanCoordList xs (x : coords)
                                 else cleanCoordList xs coords

-- | Checks that 'Coord' is present in 'CoordList'.
--
coordElem :: Eq e => Coord e -> CoordList e -> Bool
coordElem coord = any (compareCoords coord)

-- | Comparator for 'Coord's.
--
compareCoords :: Eq e => Coord e -> Coord e -> Bool
compareCoords (a, _) (b, _) = a == b
