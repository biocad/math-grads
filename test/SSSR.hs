{-# LANGUAGE ViewPatterns #-}
module Main where

import           Data.List               (group, nub, sort)
import           Data.Map                (Map)
import qualified Data.Map                as M

import           Test.Hspec              (Expectation, Spec, describe, hspec,
                                          it, shouldBe, shouldMatchList,
                                          shouldSatisfy)

import           Math.Grads.Algo.SSSR    (findSSSR)
import           Math.Grads.GenericGraph (GenericGraph)
import           Math.Grads.Graph        (GraphEdge, fromList)


main :: IO ()
main = hspec $ describe "SSSR" $ do
    graphSpec "graphA" graphA resultA
    graphSpec "graphB" graphB resultB
    graphSpec "graphC" graphC resultC
    graphSpec "graphD" graphD resultD


graphSpec :: String -> GenericGraph Int Int -> Map Int Int -> Spec
graphSpec name (sort . fmap sort . findSSSR -> sssr) cycleMap = it name $ do
    sssr `shouldMatchList` nub sssr
    mapM_ checkSimpleCycle sssr
    sssr `shouldSatisfy` (==) (sum . M.elems $ cycleMap) . length
    mapM_ (\(len, count) -> numCyclesOfLen len `shouldBe` count) $ M.toList cycleMap
  where
    checkSimpleCycle :: [GraphEdge Int] -> Expectation
    checkSimpleCycle = mapM_ (`shouldSatisfy` (==) 2 . length) . group . sort . concatMap (\(x, y, _) -> [x, y])

    numCyclesOfLen :: Int -> Int
    numCyclesOfLen n = length . filter ((==) n . length) $ sssr


-- | This 4 graphs comes from figure 5 page 5 of the original paper.
-- <https://www.ncbi.nlm.nih.gov/pubmed/19805142>
--

-- | Note that this graph image wrong in the paper - it has 3 cycles of length 4.
-- We fix it with 3 additional edges to match with results from table 4 page 5 of the paper.
--
graphA :: GenericGraph Int Int
graphA = fromList (
  [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],

  [(0,1,1),(0,5,1),(0,10,1),(0,13,1),(0,20,1),(1,2,1),(1,3,1),(1,4,1),(1,5,1),(1,20,1),(2,3,1),
  (2,8,1),(2,10,1),(2,20,1),(3,4,1),(3,8,1),(3,14,1),(3,15,1),(4,5,1),(4,15,1),(4,16,1),(4,19,1),
  (5,6,1),(5,13,1),(5,19,1),(6,7,1),(6,12,1),(6,13,1),(6,18,1),(6,19,1),(7,8,1),(7,9,1),(7,11,1),
  (7,12,1),(7,14,1),(7,17,1),(7,18,1),(8,9,1),(8,10,1),(8,14,1),(9,10,1),(9,11,1),(10,11,1),
  (10,12,1),(10,13,1),(11,12,1),(12,13,1),(14,15,1),(14,17,1),(15,16,1),(15,17,1),(16,17,1),
  (16,18,1),(16,19,1),(17,18,1),(18,19,1)])

resultA :: Map Int Int
resultA = M.fromList [(3, 36)]


graphB :: GenericGraph Int Int
graphB = fromList (
  [0,1,2,3,4,5,6,7,8,9,10,11,12],

  [(0,1,1),(0,10,1),(1,2,1),(1,8,1),(1,12,1),(2,3,1),(2,7,1),(3,4,1),(3,12,1),(4,5,1),(4,11,1),
  (5,6,1),(5,10,1),(6,7,1),(6,9,1),(7,8,1),(8,9,1),(9,10,1),(10,11,1),(11,12,1)])

resultB :: Map Int Int
resultB = M.fromList [(4, 6), (5, 2)]


graphC :: GenericGraph Int Int
graphC = fromList (
  [0,1,2,3,4,5,6,7,8,9,10,11,12],

  [(0,1,1),(0,12,1),(1,2,1),(1,8,1),(1,9,1),(1,10,1),(2,3,1),(2,4,1),(2,9,1),(3,4,1),(3,5,1),
  (3,9,1),(4,5,1),(4,12,1),(5,6,1),(5,12,1),(6,7,1),(6,11,1),(6,12,1),(7,8,1),(7,10,1),(7,11,1),
  (8,9,1),(8,10,1),(10,11,1),(11,12,1)])

resultC :: Map Int Int
resultC = M.fromList [(3, 12), (5, 2)]


graphD :: GenericGraph Int Int
graphD = fromList (
  [0,1,2,3,4,5,6,7,8,9],

  [(0,1,1),(0,3,1),(1,2,1),(1,9,1),(2,3,1),(2,9,1),(3,4,1),(3,5,1),(4,5,1),(4,6,1),(5,6,1),(6,7,1),
  (6,8,1),(7,8,1),(7,9,1),(8,9,1)])

resultD :: Map Int Int
resultD = M.fromList [(3, 5), (4, 1), (6, 1)]
