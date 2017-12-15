module Main where

import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as M
import           Math.Grads.Algo.Isomorphism (isIsoSub)
import           Math.Grads.GenericGraph     (GenericGraph)
import           Math.Grads.Graph            (fromList)
import           Test.Hspec

pathToGraphs :: FilePath
pathToGraphs = "data/Graphs.txt"

testMap :: IO (Map String (GenericGraph Int Int))
testMap = do
    graphsInLines <- lines <$> readFile pathToGraphs
    let graphsInWords = fmap words graphsInLines

    let forMap = fmap (\(x : y : _) -> (x, fromList (read y))) graphsInWords
    return (M.fromList forMap)

pathGraph :: GenericGraph Int Int
pathGraph = fromList ([0, 0, 0, 0, 0, 0, 0], [(0, 1, 1), (0, 2, 1), (0, 3, 1), (0, 4, 1), (4, 5, 1), (4, 6, 1)])

conjugatedCycles :: GenericGraph Int Int
conjugatedCycles = fromList ( [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
                           , [ (0, 1, 1), (0, 2, 1), (1, 3, 1), (2, 4, 1), (4, 5, 1), (3, 5, 1), (3, 6, 1), (5, 7, 1)
                             , (6, 8, 1), (7, 9, 1), (8, 9, 1), (1, 10, 1), (6, 11, 1), (10, 12, 1), (11, 12, 1)
                             ]
                           )

connectedCycles :: GenericGraph Int Int
connectedCycles = fromList ( [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
                            , [ (0, 1, 1), (0, 2, 1), (1, 3, 1), (2, 4, 1), (4, 5, 1), (3, 5, 1), (3, 6, 1), (6, 7, 1)
                              , (6, 8, 1), (7, 9, 1), (8, 10, 1), (9, 11, 1), (10, 11, 1), (8, 12, 1)
                            ]
                          )

cycleAndTriangle :: GenericGraph Int Int
cycleAndTriangle = fromList ( [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
                            , [ (0, 1, 1), (0, 2, 1), (1, 3, 1), (2, 4, 1), (3, 4, 1), (4, 5, 1), (5, 6, 1), (6, 7, 1)
                              , (7, 8, 1), (7, 9, 1), (8, 9, 1)
                            ]
                          )

triangleAndTriangle :: GenericGraph Int Int
triangleAndTriangle = fromList ( [0, 0, 0, 0, 0, 0, 0]
                               , [(0, 1, 1), (0, 2, 1), (1, 2, 1), (1, 3, 1), (3, 4, 1), (3, 5, 1), (4, 5, 1)]
                               )

testIsIsoSub :: SpecWith ()
testIsIsoSub = describe "Check whether subgraph isomorphism algorithm is working correctly" $ do
    it "Path" $ do
        graph <- fmap (M.! "only_path") testMap
        graph `shouldSatisfy` isIsoSub pathGraph
    it "Conjugated cycles" $ do
        graph <- fmap (M.! "only_cycles") testMap
        graph `shouldSatisfy` isIsoSub conjugatedCycles
    it "Connected cycles" $ do
        graph <- fmap (M.! "simple_drawing") testMap
        graph `shouldSatisfy` isIsoSub connectedCycles
    it "Conjugated cycles again" $ do
        graph <- fmap (M.! "hard_drawing") testMap
        graph `shouldSatisfy` isIsoSub conjugatedCycles
    it "Cycle and triangle" $ do
        graph <- fmap (M.! "paths_through_conjugated_cycles") testMap
        graph `shouldSatisfy` isIsoSub cycleAndTriangle
    it "Triangle and triangle. No match" $ do
        graph <- fmap (M.! "paths_through_conjugated_cycles") testMap
        graph `shouldNotSatisfy` isIsoSub triangleAndTriangle
    it "Cycle and triangle. No match" $ do
        graph <- fmap (M.! "simple_drawing") testMap
        graph `shouldNotSatisfy` isIsoSub cycleAndTriangle

main :: IO ()
main = hspec $ do
  testIsIsoSub
