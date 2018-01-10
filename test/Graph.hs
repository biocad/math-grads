module Main where

import           Data.List               (sort)
import           Math.Grads.GenericGraph (GenericGraph, removeEdges,
                                          removeVertices, subgraph)
import           Math.Grads.Graph        (fromList, incident, safeIncident,
                                          (!.), (!>), (?>))
import           Test.Hspec

main :: IO ()
main = hspec $ do
  opTests
  subgraphTests
  removalTests

vertices :: [Int]
vertices = [0, 1, 2, 5, 7, 8, 9, 10, 11, 19, 31, 78, 79, 85, 99, 60, 53, 100, 42]

edges :: [(Int, Int, Int)]
edges = [(0, 1, 1), (1, 2, 3), (1, 1, 2), (0, 2, 4), (3, 4, 2), (3, 5, 1),
         (5, 6, 2), (5, 7, 5), (7, 8, 3), (0, 9, 7), (0, 10, 2), (10, 11, 2),
         (10, 13, 565), (13, 14, 546), (15, 14, 42), (15, 16, -4), (16, 17, 0),
         (18, 16, 1), (16, 12, 0), (12, 10, 1)]

graph :: GenericGraph Int Int
graph = fromList (vertices, edges)

opTests :: Spec
opTests = describe "Operations on graphs." $ do
  it "Adjacent to 0." $ sort (graph !> 0) `shouldBe` [(1, 1), (2, 4), (19, 7), (31, 2)]
  it "Adjacent to 1." $ sort <$> (graph ?> 1) `shouldBe` Just [(0, 1), (1, 2), (2, 3)]
  it "Adjacent to 5." $ sort (graph !> 5) `shouldBe` [(7, 2), (8, 1)]
  it "Adjacent to 14." $ (graph ?> 14) `shouldBe` Nothing
  it "Edges incident to 8." $ sort (graph `incident` 8) `shouldBe` [(8, 5, 1), (8, 9, 2), (8, 10, 5)]
  it "Edges incident to 53." $ sort <$> (graph `safeIncident` 53) `shouldBe` Just [(53, 42, 1), (53, 60, -4), (53, 79, 0), (53, 100, 0)]

subgraphTests :: Spec
subgraphTests = describe "Subgraph tests." $ do
  let subg = graph `subgraph` [0, 3, 5, 7, 8, 11, 14]
  it "Adjacent to 0." $ subg !> 0 `shouldBe` []
  it "Adjacent to 3." $ subg !. 1 `shouldBe` [(2, 1)]
  it "Adjacent to 8." $ sort (subg !. 2) `shouldBe` [(1, 1), (3, 5)]

removalTests :: Spec
removalTests = describe "Remove operations tests." $ do
  let g1 = graph `removeEdges` [(5, 6), (15, 14), (0, 10), (10, 12)]
  let g2 = graph `removeVertices` [1, 3, 5, 7, 10, 15]
  it "Adjacent to 0." $ sort (g1 !> 0) `shouldBe` [(1, 1), (2, 4), (19, 7)]
  it "Adjacent to 31." $ sort (g1 !> 31) `shouldBe` [(78, 2), (85, 565)]
  it "Edges incident to 8." $ sort (g1 `incident` 8) `shouldBe` [(8, 5, 1), (8, 10, 5)]
  it "Adjacent to 14." $ (g1 ?> 14) `shouldBe` Nothing
  it "Adjacent to 0." $ sort (g2 !> 0) `shouldBe` [(2, 4), (19, 7)]
  it "Adjacent to 10." $ g2 ?> 31 `shouldBe` Nothing
  it "Adjacent to 11." $ g2 !> 78 `shouldBe` []
