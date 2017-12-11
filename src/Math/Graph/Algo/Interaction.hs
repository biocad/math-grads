module Math.Graph.Algo.Interaction
  ( areAdjacent
  , doubleEdgeList
  , edgeListToMap
  , getEdgeIncident
  , getEnds
  , getIndices
  , getOtherEnd
  , getSharedVertex
  , getVertexAdjacent
  , getVertexIncident
  , getVertexIncidentIdx
  , haveSharedVertex
  , haveSharedEdge
  , isIncident
  , matchEdges
  , sortBondList
  , (~=)
  , (/~=)
  ) where

import           Data.List        (find, findIndices, intersect, sortOn)
import           Data.Map         (Map)
import qualified Data.Map         as M
import           Data.Maybe       (fromJust, isJust)

import           Math.Graph.Class (EdgeList, GraphEdge, edgeType)
import           Math.Graph.Utils (nub)

(~=) :: GraphEdge e -> GraphEdge e -> Bool
(b, e, _) ~= (b', e', _) = (b == b' && e == e') || (b == e' && e == b')

(/~=) :: GraphEdge e -> GraphEdge e -> Bool
b1 /~= b2 = not $ b1 ~= b2

-- VERTEX FUNCTIONS
isIncident :: GraphEdge e -> Int -> Bool
isIncident (b, e, _) n = b == n || e == n

getVertexIncident :: [GraphEdge e] -> Int -> [GraphEdge e]
getVertexIncident bs n = filter (`isIncident` n) bs

getVertexIncidentIdx :: [GraphEdge e] -> Int -> [Int]
getVertexIncidentIdx bs n = findIndices (`isIncident` n) bs

getVertexAdjacent :: [GraphEdge e] -> Int -> [Int]
getVertexAdjacent bs n = (`getOtherEnd` n) <$> getVertexIncident bs n

areAdjacent :: [GraphEdge e] -> Int -> Int -> Bool
areAdjacent bs n n' = n' `elem` getVertexAdjacent bs n

getEnds :: GraphEdge e -> [Int]
getEnds (b, e, _) = [b, e]

haveSharedVertex :: GraphEdge e -> GraphEdge e -> Bool
haveSharedVertex b1 b2 = isJust $ getSharedVertex b1 b2

getSharedVertex :: GraphEdge e -> GraphEdge e -> Maybe Int
getSharedVertex b1 b2 | null is = Nothing
                      | length is == 2 = Nothing
                      | otherwise = Just $ head is
  where
    is = getEnds b1 `intersect` getEnds b2

toVertex :: EdgeList e -> Int -> (Int, [Int])
toVertex bonds i = (i, concatMap (\(a, b, _) -> [a | b == i]) bonds)

-- EDGE FUNCTIONS
matchEdges :: EdgeList e -> [(Int, Int)] -> EdgeList e
matchEdges bonds = fmap (\(a, b) -> fromJust (find (\bd -> bd ~= (a, b, undefined)) bonds))

getEdgeIncident :: Ord e => [GraphEdge e] -> Int -> [GraphEdge e]
getEdgeIncident bs n | n >= length bs = fail ""
                     | otherwise = filter (/= (beg, end, typ)) $ getVertexIncident bs beg ++ getVertexIncident bs end
  where
    (beg, end, typ) = bs !! n

-- TODO: get rid of undefined
getOtherEnd :: GraphEdge e -> Int -> Int
getOtherEnd (b, e, _) n | b == n = e
                        | e == n = b
                        | otherwise = undefined

-- EDGE LIST FUNCTIONS
doubleEdgeList :: EdgeList e -> EdgeList e
doubleEdgeList = concatMap (\(a, b, t) -> [(a, b, t), (b, a, t)])

edgeListToMap :: EdgeList e -> Map Int [Int]
edgeListToMap bonds' =
  let
    bonds = doubleEdgeList bonds'
  in
    M.fromList (fmap (toVertex bonds) (getIndices bonds))

haveSharedEdge :: Eq e => [GraphEdge e] -> [GraphEdge e] -> Bool
haveSharedEdge b1 b2 = or $ fmap (`elem` b2) b1

sortBondList :: Ord e => [GraphEdge e] -> [GraphEdge e]
sortBondList = sortOn left . sortOn right . sortOn edgeType
  where
    left (a, _, _) = a
    right (_, b, _) = b

-- Gets all vertices from list of bonds
getIndices :: [GraphEdge e] -> [Int]
getIndices = nub . concatMap getEnds
