{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Math.Grads.Algo.Isomorphism.RI
  ( getMultiIso
  ) where

import           Data.Array                        (Array)
import qualified Data.Array                        as A
import           Data.Bimap                        (Bimap)
import qualified Data.Bimap                        as BM
import           Data.List                         (delete, find, maximumBy)
import           Data.Map                          (Map)
import           Data.Ord                          (comparing)

import           Math.Grads.Algo.Isomorphism.Types (GComparable (..))
import           Math.Grads.GenericGraph           (GenericGraph, gAdjacency,
                                                    getEdge)


-- | RI isomorphism algorithm.
-- <https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-14-S7-S13>

type AdjArr  = Array Int [Int]
type VComp   = (Int -> Int -> Bool)
type EComp   = ((Int, Int) -> (Int, Int) -> Bool)


getMultiIso :: Ord v1 => Eq e1 => Ord v2 => Eq e2
            => GComparable GenericGraph v1 e1 GenericGraph v2 e2
            => GenericGraph v1 e1 -> GenericGraph v2 e2
            -> [Map Int Int]
getMultiIso queryGraph@(toAdjArr -> queryAdjArr) targetGraph@(toAdjArr -> targetAdjArr) = isos
  where
    (order, parents) = buildMatchingOrder queryAdjArr
    isos             = findIsos queryAdjArr targetAdjArr vComp eComp order parents

    vComp = vComparator queryGraph targetGraph
    eComp = eIndComp
      where
        comp                   = eComparator queryGraph targetGraph
        eIndComp (a, b) (x, y) = comp (a, b, getEdge queryGraph a b) (x, y, getEdge targetGraph x y)

buildMatchingOrder :: AdjArr -> ([Int], [Maybe Int])
buildMatchingOrder graph = buildOrder [] [] $ A.indices graph
  where
    buildOrder :: [Int] -> [Maybe Int] -> [Int] -> ([Int], [Maybe Int])
    buildOrder visited parents unvisited
      | null unvisited = (visited, parents)
      | otherwise      = buildOrder nextVisited nextParents nextUnvisited
      where
        maxVertex       = maximumBy (comparing vertexRank) unvisited
        maxVertexParent = find ((maxVertex `elem`) . (graph !.)) visited

        nextVisited   = visited ++ [maxVertex]
        nextParents   = parents ++ [maxVertexParent]
        nextUnvisited = delete maxVertex unvisited

        vertexRank :: Int -> (Int, Int, Int)
        vertexRank ind
          | null visited = (degree,  0,       0)
          | otherwise    = (visRank, neiRank, unvisRank)
          where
            neis      = graph !. ind
            neisUnvis = filter (`notElem` visited) neis

            degree    = length neis
            visRank   = count neisWithInd visited
            neiRank   = count neisWithInd1Unvis visited
            unvisRank = count notNeiWithVis neisUnvis

            neisWithInd       = (ind `elem`) . (graph !.)
            neisWithInd1Unvis = any (`elem` neisUnvis) . (graph !.)
            notNeiWithVis     = all (`notElem` visited) . (graph !.)

findIsos :: AdjArr -> AdjArr -> VComp -> EComp
         -> [Int] -> [Maybe Int]
         -> [Map Int Int]
findIsos queryGraph targetGraph vComp eComp order parents =
  BM.toMap <$> goRI order parents BM.empty
  where
    goRI :: [Int] -> [Maybe Int] -> Bimap Int Int -> [Bimap Int Int]
    goRI []            _             match = [match]
    goRI _             []            match = [match]
    goRI (queryV : vs) (parent : ps) match = concatMap (goRI vs ps) matches
      where
        matches :: [Bimap Int Int]
        matches = (\targetV -> BM.insert queryV targetV match) <$> targetVs

        targetVs :: [Int]
        targetVs = filter isValidMatch $ maybe targetVsUnmatched findCandidates parent

        targetVsUnmatched :: [Int]
        targetVsUnmatched = filter (`BM.notMemberR` match) $ A.indices targetGraph

        findCandidates :: Int -> [Int]
        findCandidates = filter (`BM.notMemberR` match) . (targetGraph !.) . (match BM.!)

        isValidMatch :: Int -> Bool
        isValidMatch targetV =
          vComp queryV targetV &&
          length queryNeis <= length targetNeis &&
          all (`elem` targetNeisMatched) queryNeisMatchedProjection &&
          all (uncurry eComp) matchedEdges
          where
            queryNeis  = queryGraph  !. queryV
            targetNeis = targetGraph !. targetV

            queryNeisMatched           = filter (`BM.member` match) queryNeis
            queryNeisMatchedProjection = (match BM.!) <$> queryNeisMatched
            targetNeisMatched          = filter (`BM.memberR` match) targetNeis

            matchedEdges :: [((Int, Int), (Int, Int))]
            matchedEdges = matchedEdgePair <$> queryNeisMatched
              where
                matchedEdgePair queryNei = ((queryV, queryNei), (targetV, match BM.! queryNei))

toAdjArr :: GenericGraph v b -> Array Int [Int]
toAdjArr = fmap (fmap fst) . gAdjacency

(!.) :: AdjArr -> Int -> [Int]
(!.) = (A.!)

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p
